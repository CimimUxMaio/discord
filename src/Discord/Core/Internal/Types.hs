{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}

module Discord.Core.Internal.Types where

import Control.Monad.Writer (Writer, MonadWriter (tell, writer), runWriter, WriterT)
import Control.Monad.Reader (ReaderT (runReaderT), ask, MonadTrans (lift), MonadIO (liftIO), Reader, MonadReader (reader), runReader)
import Data.Text (Text)
import qualified Data.Text as Text
import Discord.API.Internal.Types.BotEvent (BotEvent)
import Control.Applicative (Alternative)
import GHC.Base (Alternative(empty, (<|>)))
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.State (StateT (runStateT))
import Control.Monad.RWS (MonadState)
import Control.Exception (Exception)
import Discord.Core.Internal.Parsers (BotEventParser)
import Discord.Core.Context (Context)

-- | Defines how to handle a given exception given the context in which it was thrown.
{-
__Example:__

exceptionHandlers :: [BotExceptionHandler s]
exceptionHandlers = 
    [ BotExceptionHandler $ \ctx (e :: ArithException) -> do 
        liftIO $ print "Recovering..."
        case ctx of
            CommandCtx name args msg -> do
                let chid = messageChannelId msg
                void . sendText chid $ "Something went wrong!"
            _ -> pure () 
    ]
-}
data BotExceptionHandler s =
    forall e. Exception e => BotExceptionHandler (Context -> e -> BotAction s ())


-- | Bot configuration.
{-
__Example:__

cfg :: BotConfig
cfg = BotConfig
    { prefix = "?"
    , token  = "<your_token>"
    }
-}
data BotConfig = BotConfig
    { prefix :: Text  -- ^ Prefix to be used on commands
    , token  :: Text  -- ^ Discord api token
    } deriving (Show)


-- | Bot application definition.
{-
__Example:__

newtype CustomAppState = CustomAppState Int deriving (Show, Num)


app :: BotConfig -> BotApp CustomAppState
app cfg = BotApp
    { appConfig            = cfg 
    , appInitialState      = CustomAppState 0
    , appHandler           = customHandler 
    , appExceptionHandlers = exceptionHandlers
    }


exceptionHandlers :: [BotExceptionHandler s]
exceptionHandlers = 
    [ BotExceptionHandler $ \ctx (e :: ArithException) -> do 
        liftIO $ print "Recovering..."
        case ctx of
            CommandCtx name args msg -> do
                let chid = messageChannelId msg
                void . sendText chid $ "Something went wrong!"
            _ -> pure () 
    ]
    

customEmbed :: Maybe User -> Embed
customEmbed blame = runEmbedBuilder $ do 
    title "A nice title"
    description "a nicer embed description"
    color tomato


customHandler :: BotM CustomAppState ()
customHandler = do
    onCommand "ping" $ \msg args -> do
        let chid = messageChannelId msg
        void $ sendText chid "Pong!"

    onCommand "fail" $ \_ _ -> do
        throw DivideByZero


main :: IO ()
main = do
    botToken <- getEnv "DISCORD_TOKEN"
    let cfg = BotConfig { prefix = "?", token  = pack botToken }
    startBot (app cfg)
-}
data BotApp s = BotApp
    { appConfig            :: BotConfig                -- ^ Configuration
    , appInitialState      :: s                        -- ^ Initial state
    , appExceptionHandlers :: [BotExceptionHandler s]  -- ^ How to handle exceptions
    , appHandler           :: BotM s ()                -- ^ Main application definition
    }


type BotHandlerEventParser s = BotEventParser (Context, BotAction s ())

-- | Used for defining the application handler.
-- See example at 'BotApp'.
newtype BotM s a = BotM { _runBotM :: ReaderT BotConfig (Writer [BotHandlerEventParser s]) a }
    deriving (Functor, Applicative, Monad, MonadReader BotConfig, MonadWriter [BotHandlerEventParser s])

runBotM :: BotM s a -> BotConfig -> [BotHandlerEventParser s]
runBotM m = snd . runWriter . runReaderT (_runBotM m)


-- | Used for defining actions to be executed when handling events.
-- See example at 'BotApp'.
newtype BotAction s a = BotAction { _runBotAction :: ReaderT BotConfig (StateT s IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader BotConfig, MonadState s)

runBotAction :: BotConfig -> s -> BotAction s a -> IO (a, s)
runBotAction cfg state = (`runStateT` state) . (`runReaderT` cfg) . _runBotAction

runBotAction_ :: BotConfig -> s -> BotAction s () -> IO s
runBotAction_ cfg state = (snd <$>) . runBotAction cfg state
