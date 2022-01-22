{-# LANGUAGE DeriveFunctor #-}

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


data BotApp s = BotApp
    { appConfig       :: BotConfig
    , appInitialState :: s
    , appHandler      :: BotM s ()
    }

botAppEventHandler :: BotApp s -> BotEvent -> BotAction s ()
botAppEventHandler app event =
    fromMaybe (pure ()) . ($ event) . runBotEventParser . asum $ runBotM (appHandler app) (appConfig app)


data BotConfig = BotConfig
    { prefix :: Text
    , token  :: Text
    } deriving (Show)


type BotActionEventParser_ s = BotEventParser (BotAction s ())

newtype BotM s a = BotM { _runBotM :: ReaderT BotConfig (Writer [BotActionEventParser_ s]) a }
    deriving (Functor, Applicative, Monad, MonadReader BotConfig, MonadWriter [BotActionEventParser_ s])

runBotM :: BotM s a -> BotConfig -> [BotEventParser (BotAction s ())]
runBotM m = snd . runWriter . runReaderT (_runBotM m)


newtype BotAction s a = BotAction { _runBotAction :: ReaderT BotConfig (StateT s IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader BotConfig, MonadState s)

runBotAction :: BotConfig -> s -> BotAction s a -> IO (a, s)
runBotAction cfg state = (`runStateT` state) . (`runReaderT` cfg) . _runBotAction


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


