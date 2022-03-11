{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}

module Discord.Core.Internal.Types where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS.Class (MonadReader)
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Control.Concurrent.STM (TVar, modifyTVar, atomically, readTVarIO, TMVar)
import Control.Monad (void)
import Control.Exception (Exception)
import Discord.Core.Context (Context)
import Data.Text (Text)
import Discord.API.Internal.Types.BotEvent (BotEvent)
import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad.Writer (Writer, MonadWriter)
import Control.Concurrent (ThreadId)



newtype BotEventParser a = BotEventParser
    { runBotEventParser :: BotEvent -> Maybe a }
    deriving Functor

instance Applicative BotEventParser where
    pure x = BotEventParser (pure . pure $ x)
    BotEventParser f <*> BotEventParser x = BotEventParser $ \e -> f e <*> x e

instance Alternative BotEventParser where
    empty = BotEventParser $ const Nothing
    BotEventParser f <|> BotEventParser g = BotEventParser $ \e -> f e <|> g e

instance Monad BotEventParser where
    return = pure
    BotEventParser x >>= f = BotEventParser $ \e -> x e >>= flip runBotEventParser e . f

instance MonadFail BotEventParser where
    fail _ = empty




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
data BotApp s = BotApp
    { appConfig            :: BotConfig                -- ^ Configuration
    , appInitialState      :: s                        -- ^ Initial state
    , appExceptionHandlers :: [BotExceptionHandler s]  -- ^ How to handle exceptions
    , appDefinition        :: BotM s ()                -- ^ Main application definition
    }


type BotHandlerEventParser s = BotEventParser (Context, BotAction s ())

-- | Used for defining the application handler.
-- See example at 'BotApp'.
newtype BotM s a = BotM { _runBotM :: ReaderT BotConfig (Writer [BotHandlerEventParser s]) a }
    deriving (Functor, Applicative, Monad, MonadReader BotConfig, MonadWriter [BotHandlerEventParser s])



data WaitingTask = WaitingTask
    { wtMatches   :: BotEvent -> Bool 
    , wtContainer :: TMVar (Maybe BotEvent)
    , wtThreadId  :: ThreadId
    }


-- | Environment in which a BotAction is running.
data BotEnv s = BotEnv
    { envConfig        :: BotConfig  -- ^ the 'BotConfig' being used
    , envState         :: TVar s     -- ^ the application's state
    , _envWaitingTasks :: TVar [WaitingTask]
    }


-- | Used for defining actions to be executed when handling events.
-- See example at 'BotApp'.
newtype BotAction s a = BotAction { _runBotAction :: ReaderT (BotEnv s) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (BotEnv s))
