{-# LANGUAGE DeriveFunctor #-}

module Discord.Core.Internal.Types where
import Control.Monad.Writer (Writer, MonadWriter (tell, writer), runWriter, WriterT)
import Control.Monad.Reader (ReaderT (runReaderT), ask, MonadTrans (lift), MonadIO (liftIO), Reader, MonadReader (reader))
import Data.Text (Text)
import qualified Data.Text as Text
import Discord.API.Internal.Types.BotEvent (BotEvent)
import Control.Applicative (Alternative)
import GHC.Base (Alternative(empty, (<|>)))
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)


data BotApp = BotApp
    { config :: BotConfig
    , handler :: BotM }

botAppEventHandler :: BotApp -> BotEvent -> BotAction
botAppEventHandler app event =
    fromMaybe (pure ()) . ($ event) . runBotEventParser . asum $ runBotM (handler app) (config app)

data BotConfig = BotConfig
    { prefix :: Text
    , token  :: Text
    } deriving (Show)


type BotM = ReaderT BotConfig (Writer [BotEventParser BotAction]) ()

runBotM :: BotM -> BotConfig -> [BotEventParser BotAction]
runBotM m = snd . runWriter . runReaderT m


type BotAction = ReaderT BotConfig IO ()

runBotAction :: BotConfig -> BotAction -> IO ()
runBotAction cfg action = runReaderT action cfg

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


