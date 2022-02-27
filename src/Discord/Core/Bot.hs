module Discord.Core.Bot
( startBot
, Types.BotApp
, Types.BotConfig
, Types.BotExceptionHandler ) where

import Data.Text (Text)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (runReader, MonadTrans (lift), MonadReader (reader), forever)
import Control.Concurrent (newChan, threadDelay, writeChan, Chan, readChan)
import Discord.Core.Internal.EventLoop (startEventLoop)
import GHC.Conc (forkIO)
import Discord.API.Internal.Types.BotEvent (BotEvent(Resumed))
import Control.Monad.Trans.Writer (tell)
import Data.Default (def)
import Discord.API.Internal.Gateway (startGatewayController)
import Discord.Core.Internal.Types
    ( BotConfig(token), BotApp(appConfig) )
import qualified Discord.Core.Internal.Types as Types


-- | Starts the bot application.
startBot :: BotApp s -> IO ()
startBot app = do
    eventQueue <- newChan
    _eventLoop <- forkIO $ startEventLoop eventQueue app
    print "bot started"
    startGatewayController (token . appConfig $ app) def eventQueue