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


data BotExceptionHandler s =
    forall e. Exception e => BotExceptionHandler (e -> BotAction s ())


data BotConfig = BotConfig
    { prefix :: Text
    , token  :: Text
    } deriving (Show)


data BotApp s = BotApp
    { appConfig            :: BotConfig
    , appInitialState      :: s
    , appExceptionHandlers :: [BotExceptionHandler s]
    , appHandler           :: BotM s ()
    }


type BotActionEventParser_ s = BotEventParser (BotAction s ())

newtype BotM s a = BotM { _runBotM :: ReaderT BotConfig (Writer [BotActionEventParser_ s]) a }
    deriving (Functor, Applicative, Monad, MonadReader BotConfig, MonadWriter [BotActionEventParser_ s])

runBotM :: BotM s a -> BotConfig -> [BotEventParser (BotAction s ())]
runBotM m = snd . runWriter . runReaderT (_runBotM m)


newtype BotAction s a = BotAction { _runBotAction :: ReaderT BotConfig (StateT s IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader BotConfig, MonadState s)

runBotAction :: BotConfig -> s -> BotAction s a -> IO (a, s)
runBotAction cfg state = (`runStateT` state) . (`runReaderT` cfg) . _runBotAction

