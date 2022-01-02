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


data BotApp = BotApp
    { config :: BotConfig
    , handler :: BotM () }

botAppEventHandler :: BotApp -> BotEvent -> BotAction ()
botAppEventHandler app event =
    fromMaybe (pure ()) . ($ event) . runBotEventParser . asum $ runBotM (handler app) (config app)


data BotConfig = BotConfig
    { prefix :: Text
    , token  :: Text
    } deriving (Show)


type BotActionEventParser_ = BotEventParser (BotAction ())

newtype BotM a = BotM { _runBotM :: ReaderT BotConfig (Writer [BotActionEventParser_]) a }
    deriving (Functor, Applicative, Monad, MonadReader BotConfig, MonadWriter [BotActionEventParser_])


runBotM :: BotM a -> BotConfig -> [BotEventParser (BotAction ())]
runBotM m = snd . runWriter . runReaderT (_runBotM m)


newtype BotAction a = BotAction { _runBotAction :: ReaderT BotConfig IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader BotConfig)

runBotAction :: BotConfig -> BotAction () -> IO ()
runBotAction cfg action = runReaderT (_runBotAction action) cfg


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


