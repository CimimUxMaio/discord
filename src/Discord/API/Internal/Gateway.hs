{-# LANGUAGE NumericUnderscores #-}

module Discord.API.Internal.Gateway where

import Discord.API.Internal.Types.Gateway (GatewayReceivable (Hello, HeartbeatRequest, Dispatch, Reconnect, InvalidSession, HeartbeatACK), GatewaySendableInternal (Heartbeat, Identify, Resume), compileGatewayIntent, GatewayIntent (GatewayIntent))
import Control.Monad.State (StateT(StateT, runStateT), forever, evalStateT, MonadIO (liftIO), MonadState (put, get), modify, gets)
import Data.Text (Text, pack, unpack)
import Network.HTTP.Req (runReq, defaultHttpConfig, req, GET (GET), jsonResponse, https, (/:), responseBody, NoReqBody (NoReqBody), HttpException)
import Data.Bifunctor (first)
import Wuss (runSecureClient)
import Data.Aeson (decode, eitherDecode, encode)
import Network.WebSockets (receiveData, ClientApp, sendTextData)
import Network.WebSockets.Connection (Connection, sendClose)
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
import Protolude (print, Chan, writeChan, when, finally, SomeException (SomeException))
import Prelude hiding (print)
import Discord.Core.Internal.Types (BotConfig(token))
import Control.Monad.Extra (whenJust)
import Control.Exception (try)


data GatewayEnv = GatewayEnv
    { gatewayToken      :: Text
    , gatewayIntent     :: GatewayIntent
    , gatewayEventQueue :: Chan BotEvent
    }


data GatewayState = GatewayState
    { gatewayHeartbeatThread   :: Maybe ThreadId
    , gatewayLastSeq           :: IORef (Maybe Integer)
    , gatewaySessionId         :: IORef (Maybe Text)
    , gatewayLastHeartbeat     :: IORef UTCTime
    }


newtype GatewayM a =
    GatewayM { _runGatewayM :: ReaderT GatewayEnv (StateT GatewayState IO) a }
    deriving (Functor, Applicative, Monad, MonadReader GatewayEnv, MonadState GatewayState, MonadIO)

runGatewayM :: GatewayEnv -> GatewayState -> GatewayM a -> IO GatewayState
runGatewayM env state m = snd <$> ((`runStateT` state) . runReaderT (_runGatewayM m) $ env)


gatewayInitialState :: IO GatewayState
gatewayInitialState = do 
    lastSeqRef <- newIORef Nothing
    time <- getCurrentTime
    lastHeartbeatRef <- newIORef time
    sessionIdRef <- newIORef Nothing

    pure GatewayState
        { gatewayHeartbeatThread   = Nothing
        , gatewayLastSeq           = lastSeqRef
        , gatewaySessionId         = sessionIdRef
        , gatewayLastHeartbeat     = lastHeartbeatRef
        }


startGatewayController :: Text -> GatewayIntent -> Chan BotEvent -> IO ()
startGatewayController token intent eventQueue = do
    let initialEnv = GatewayEnv { gatewayToken = token
                                , gatewayIntent = intent
                                , gatewayEventQueue = eventQueue
                                }

    initialState <- gatewayInitialState

    tid <- instanceGateway initialEnv initialState
    gatewayTidRef <- newIORef tid

    void . runGatewayM initialEnv initialState $ startHealthCheckLoop gatewayTidRef


instanceGateway :: GatewayEnv -> GatewayState -> IO ThreadId 
instanceGateway env = forkIO . gateway env


gateway :: GatewayEnv -> GatewayState -> IO ()
gateway env state = do
    r <- try $ do
        finalState <- runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json" $
            runGatewayM env state . gatewayEventLoop

        cleanup finalState
        gateway env finalState

    print (r :: Either SomeException ()) 

    where cleanup state = do
              let tid = gatewayHeartbeatThread state
              liftIO $ whenJust tid killThread


continueEventLoop :: Connection -> GatewayM ()
continueEventLoop = gatewayEventLoop


cleanupGateway :: GatewayM ()
cleanupGateway = do
    print "cleaning up gateway"
    tid <- gets gatewayHeartbeatThread
    liftIO $ whenJust tid killThread


gatewayEventLoop :: Connection -> GatewayM ()
gatewayEventLoop conn = do
    e <- gatewayReceive conn
    case e of
        Left parseErrorMessage -> print parseErrorMessage
        Right event -> gatewayEventHandler conn event


gatewayEventHandler :: Connection -> GatewayReceivable -> GatewayM ()
gatewayEventHandler conn e = case e of
    Hello interval    -> do
        lastSeqRef <- gets gatewayLastSeq
        tid <- liftIO $ startHeartbeatLoop lastSeqRef conn interval
        modify (\s -> s { gatewayHeartbeatThread = Just tid })
        token <- asks gatewayToken

        sessionId <- readGatewayRef gatewaySessionId
        case sessionId of
            Just sid -> attemptResume conn sid
            Nothing -> identify conn

        continueEventLoop conn

    HeartbeatRequest  -> do
        lastSeq <- readGatewayRef gatewayLastSeq
        liftIO $ sendHeartbeat lastSeq conn
        continueEventLoop conn

    Reconnect         -> exitEventLoop

    InvalidSession    -> do
        liftIO $ threadDelay 3_000_000
        identify conn
        continueEventLoop conn

    HeartbeatACK      -> do
        ref <- gets gatewayLastHeartbeat
        time <- liftIO getCurrentTime
        liftIO $ writeIORef ref time
        print "gateway acknowledged heartbeat"
        continueEventLoop conn

    Dispatch botEvent seqNum -> do
        case botEvent of
            Ready version user guilds sessionId -> do
                print "gateway ready"
                sessionIdRef <- gets gatewaySessionId
                liftIO $ writeIORef sessionIdRef (Just sessionId)

            Resumed -> print "gateway successfully resumed connection"

            _ -> pure ()

        lastSeqRef <- gets gatewayLastSeq
        liftIO $ writeIORef lastSeqRef seqNum
            
        eventChan <- asks gatewayEventQueue
        liftIO $ writeChan eventChan botEvent

        continueEventLoop conn
    
    where exitEventLoop = pure ()

          identify :: Connection -> GatewayM ()
          identify conn = do
              token <- asks gatewayToken
              intent <- asks gatewayIntent
              liftIO . sendGateway conn $ Identify token (compileGatewayIntent intent)

          attemptResume conn prevSessionId = do
              token <- asks gatewayToken
              lastSeq <- readGatewayRef gatewayLastSeq
              case lastSeq of
                  Nothing   -> identify conn
                  Just lseq -> resume conn prevSessionId lseq
          
          resume :: Connection -> Text -> Integer -> GatewayM ()
          resume conn sessionId lastSeq = do
              print "attempting to resume"
              token <- asks gatewayToken
              liftIO . sendGateway conn $ Resume token sessionId lastSeq
          

readGatewayRef :: (GatewayState -> IORef a) -> GatewayM a
readGatewayRef getter = do
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


sendGateway :: Connection -> GatewaySendableInternal -> IO ()
sendGateway conn = sendTextData conn . encode


startHealthCheckLoop :: IORef ThreadId -> GatewayM ()
startHealthCheckLoop gatewayTidRef = forever $ do
    liftIO $ threadDelay 20_000_000

    sessionId <- readGatewayRef gatewaySessionId
    lastSeq <- readGatewayRef gatewayLastSeq
    lastHeartbeat <- readGatewayRef gatewayLastHeartbeat

    now <- liftIO getCurrentTime 

    when (diffUTCTime now lastHeartbeat >= 60) $ do
        print "gateway server timed out, attempting to reconnect..."
        gatewayTid <- liftIO $ readIORef gatewayTidRef

        liftIO $ killThread gatewayTid

        env <- ask
        state <- get
        newGatewayTid <- liftIO $ instanceGateway env state
        liftIO $ writeIORef gatewayTidRef newGatewayTid