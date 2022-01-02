module Discord.Core.Bot where
import Data.Text (Text)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (runReader, MonadTrans (lift), MonadReader (reader), forever)
import Control.Concurrent (newChan, threadDelay, writeChan, Chan, readChan)
import Discord.Core.EventLoop (startEventLoop)
import GHC.Conc (forkIO)
import Discord.API.Internal.Types.BotEvent (BotEvent(Resumed))
import Discord.Core.Internal.Types (BotEventParser, BotAction, BotM, BotConfig (prefix), BotApp (config), botAppEventHandler, runBotAction)
import Control.Monad.Trans.Writer (tell)
import Discord.Core.Internal.Utils (commandParser, addParser, textParser)



startBot :: BotApp -> IO ()
startBot app = do
    eventQueue <- newChan
    _eventLoop <- forkIO $ startEventLoop eventQueue app
    print "bot started"