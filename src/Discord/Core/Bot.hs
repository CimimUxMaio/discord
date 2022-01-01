module Discord.Core.Bot where
import Data.Text (Text)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (runReader)
import Control.Concurrent (newChan, threadDelay, writeChan)
import Discord.Core.EventLoop (startEventLoop)
import GHC.Conc (forkIO)
import Control.Monad (forever)
import Discord.API.Internal.Types.BotEvent (BotEvent(Resumed))



data BotConfig = BotConfig
    { prefix :: Text
    , token  :: Text
    } deriving (Show)


type BotM a = ReaderT BotConfig IO a

-- printConfig :: BotM ()
-- printConfig = do
--     config <- ask 
--     liftIO $ print config
--
-- runBot :: BotConfig -> IO ()
-- runBot = runReaderT $ do
--     printConfig


startBot :: IO ()
startBot = do
    eventQueue <- newChan
    eventLoop <- forkIO $ startEventLoop eventQueue
    print "bot started"