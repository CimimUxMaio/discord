module Discord.Core.EventLoop where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent (readChan, Chan)
import Discord.API.Internal.Types.BotEvent (BotEvent)
import Control.Monad (forever)



startEventLoop :: Chan BotEvent -> IO ()
startEventLoop = forever . handleNext


handleNext :: Chan BotEvent -> IO ()
handleNext eventQueue = readChan eventQueue >>= undefined -- handle