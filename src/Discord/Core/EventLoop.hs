module Discord.Core.EventLoop where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent (readChan, Chan)
import Discord.API.Internal.Types.BotEvent (BotEvent)
import Control.Monad (forever)
import Discord.Core.Internal.Types (BotApp(BotApp, config), runBotAction, botAppEventHandler)



startEventLoop :: Chan BotEvent -> BotApp -> IO ()
startEventLoop eventQueue app = forever $ do
    event <- readChan eventQueue
    eventHandler event

    where eventHandler = runBotAction cfg . botAppEventHandler app
          cfg = config app