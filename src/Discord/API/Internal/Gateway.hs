{-# LANGUAGE NumericUnderscores #-}

module Discord.API.Internal.Gateway where

import Discord.API.Internal.Types.Gateway (GatewayReceivable (Hello, HeartbeatRequest, Dispatch, Reconnect, InvalidSession, HeartbeatACK), GatewaySendableInternal (Heartbeat, Identify, Resume), compileGatewayIntent, GatewayIntent)
import Control.Monad.State (StateT(StateT, runStateT), forever, evalStateT, MonadIO (liftIO), MonadState (put, get), modify, gets, when)
import Data.Text (Text, pack, unpack)
import Network.HTTP.Req (runReq, defaultHttpConfig, req, GET (GET), jsonResponse, https, (/:), responseBody, NoReqBody (NoReqBody), HttpException)
import Control.Exception (try, catch, handle)
import Data.Bifunctor (first)
import Wuss (runSecureClient)
import Data.Aeson (decode, eitherDecode, encode)
import Network.WebSockets (receiveData, ClientApp, sendTextData)
import Network.WebSockets.Connection (Connection)
import Control.Monad (void, (<=<))
import GHC.Conc (forkIO, ThreadId, threadDelay, killThread)
import Data.IORef (IORef, readIORef, modifyIORef, writeIORef, newIORef)
import Data.Maybe (fromMaybe, fromJust)
import Data.Functor ((<&>))
import Data.Default (Default(def))
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Control.Monad.RWS (MonadReader (ask), asks)
import Discord.API.Internal.Types.BotEvent (BotEvent(Ready, Resumed))
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Protolude (print, Chan, writeChan)
import Prelude hiding (print)
import Discord.Core.Internal.Types (BotConfig(token))
import GHC.Num (integerToInt)
import Control.Monad.Extra (whenJust)


data GatewayEnv = GatewayEnv
    { gatewayToken      :: Text
    , gatewayIntent     :: GatewayIntent
    , gatewayEventQueue :: Chan BotEvent
    }

data GatewayState = GatewayState
    { gatewayHeartbeatThread   :: Maybe ThreadId
    , gatewayHealthCheckThread :: Maybe ThreadId
    , gatewayLastSeq           :: IORef (Maybe Integer)
    , gatewaySessionId         :: IORef (Maybe Text)
    , gatewayLastHeartbeat     :: IORef UTCTime
    }


newtype GatewayM a =
    GatewayM { _runGatewayM :: ReaderT GatewayEnv (StateT GatewayState IO) a }
    deriving (Functor, Applicative, Monad, MonadReader GatewayEnv, MonadState GatewayState, MonadIO)

runGatewayM :: GatewayEnv -> GatewayState -> GatewayM a -> IO ()
runGatewayM env state m = void . (`runStateT` state) . runReaderT (_runGatewayM m) $ env


gatewayInitialState :: IO GatewayState
gatewayInitialState = do 
    lastSeqRef <- newIORef Nothing
    time <- getCurrentTime
    lastHeartbeatRef <- newIORef time
    sessionIdRef <- newIORef Nothing

    pure GatewayState
        { gatewayHeartbeatThread   = Nothing
        , gatewayHealthCheckThread = Nothing
        , gatewayLastSeq           = lastSeqRef
        , gatewaySessionId         = sessionIdRef
        , gatewayLastHeartbeat     = lastHeartbeatRef
        }


startGateway :: Text -> GatewayIntent -> Chan BotEvent -> IO ()
startGateway token intent eventQueue = do
    let initialEnv = GatewayEnv { gatewayToken = token
                                , gatewayIntent = intent
                                , gatewayEventQueue = eventQueue
                                }

    initialState <- gatewayInitialState
    runGatewayM initialEnv initialState gateway


gateway :: GatewayM ()
gateway = do
    cleanThreads

    env <- ask
    let token = gatewayToken env

    state <- get
    let sessionIdRef = gatewaySessionId state
    let lastSeqRef = gatewayLastSeq state
    let lastHeartbeatRef = gatewayLastHeartbeat state

    liftIO $ runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json" $ \conn -> do
        tid <- startHealthCheckLoop token sessionIdRef lastHeartbeatRef lastSeqRef conn
        let updatedState = state { gatewayHealthCheckThread = Just tid }
        gatewayEventLoop env updatedState conn

    where cleanThreads = do
            hbTid <- gets gatewayHeartbeatThread
            hcTid <- gets gatewayHealthCheckThread
            liftIO $ whenJust hbTid killThread
            liftIO $ whenJust hcTid killThread


continueGateway :: Connection -> GatewayM ()
continueGateway conn = do
    env <- ask
    currentState <- get
    liftIO $ gatewayEventLoop env currentState conn


gatewayEventLoop :: GatewayEnv -> GatewayState -> ClientApp ()
gatewayEventLoop env state conn = runGatewayM env state $ do
    e <- gatewayReceive conn
    case e of
        Left parseErrorMessage -> print parseErrorMessage >> continueGateway conn
        Right event -> gatewayEventHandler conn event


gatewayEventHandler :: Connection -> GatewayReceivable -> GatewayM ()
gatewayEventHandler conn e = case e of
    Hello interval    -> do
        lastSeqRef <- gets gatewayLastSeq
        tid <- liftIO $ startHeartbeatLoop lastSeqRef conn interval
        modify (\s -> s { gatewayHeartbeatThread = Just tid })
        token <- asks gatewayToken
        print interval
        liftIO $ sendIdentify token def conn
        continueGateway conn

    HeartbeatRequest  -> do
        lastSeq <- getRef gatewayLastSeq
        liftIO $ sendHeartbeat lastSeq conn
        continueGateway conn

    Reconnect         -> gateway

    InvalidSession    -> do
        liftIO $ threadDelay 3_000_000
        token <- asks gatewayToken
        intent <- asks gatewayIntent
        liftIO $ sendIdentify token intent conn
        continueGateway conn

    HeartbeatACK      -> do
        ref <- gets gatewayLastHeartbeat
        time <- liftIO getCurrentTime
        liftIO $ writeIORef ref time
        print "gateway acknowledged heartbeat"
        continueGateway conn

    Dispatch botEvent -> do
        case botEvent of
            Ready version user guilds sessionId -> do
                sessionIdRef <- gets gatewaySessionId
                liftIO $ writeIORef sessionIdRef (Just sessionId)
                print "gateway ready"

            Resumed -> print "gateway successfully resumed connection"
            _ -> do
                eventChan <- asks gatewayEventQueue
                liftIO $ writeChan eventChan botEvent

        continueGateway conn
          

getRef :: (GatewayState -> IORef a) -> GatewayM a
getRef getter = do
    ref <- gets getter
    liftIO $ readIORef ref


gatewayReceive :: Connection -> GatewayM (Either Text GatewayReceivable)
gatewayReceive conn = do
    msg <- liftIO $ receiveData conn
    pure (first pack $ eitherDecode msg :: Either Text GatewayReceivable)


startHeartbeatLoop :: IORef (Maybe Integer) -> Connection -> Int -> IO ThreadId
startHeartbeatLoop lastSeqRef conn interval = forkIO . forever $ do
    lastSeq <- readIORef lastSeqRef
    sendHeartbeat lastSeq conn
    print "gateway heartbeat sent"
    threadDelay (interval * 1000)


sendHeartbeat :: Maybe Integer -> Connection -> IO ()
sendHeartbeat lastSeq conn = do
    sendTextData conn $ encode (Heartbeat lastSeq)


sendIdentify :: Text -> GatewayIntent -> Connection -> IO ()
sendIdentify token intent conn = do
    sendTextData conn $ encode (Identify token (compileGatewayIntent intent))


startHealthCheckLoop :: Text -> IORef (Maybe Text) -> IORef UTCTime -> IORef (Maybe Integer) -> Connection -> IO ThreadId
startHealthCheckLoop token sessionIdRef lastHeartbeatRef lastSeqRef conn = forkIO . forever $ do
    threadDelay 20_000_000

    sessionId <- readIORef sessionIdRef
    lastSeq <- readIORef lastSeqRef
    lastHeartbeat <- readIORef lastHeartbeatRef
    now <- getCurrentTime 

    when (diffUTCTime now lastHeartbeat >= 60) $ do
        print "gateway server timed out, attempting to reconnect..."
        maybe 
            (print "gateway could not resume connection") 
            ((\_ -> print "gateway resume request sent") <=< sendTextData conn . encode) 
            (Resume token <$> sessionId <*> lastSeq)

    where sendResume resume = sendTextData conn $ encode resume