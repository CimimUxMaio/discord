{-# LANGUAGE RankNTypes #-}
module Discord.Core.EventLoop (startEventLoop) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent (readChan, Chan)
import Discord.API.Internal.Types.BotEvent (BotEvent)
import Discord.Core.Internal.Types (BotApp(BotApp, appConfig, appInitialState), runBotAction, botAppEventHandler)



startEventLoop :: Chan BotEvent -> BotApp s -> IO ()
startEventLoop eventQueue app = loop eventQueue eventHandler initialState
    where eventHandler currentState = runBotAction cfg currentState . botAppEventHandler app
          cfg = appConfig app
          initialState = appInitialState app


loop :: Chan BotEvent -> (s -> BotEvent -> IO ((), s)) -> s -> IO ()
loop eventQueue eventHandler currentState = do
    event <- readChan eventQueue
    (_, finalState) <- eventHandler currentState event
    loop eventQueue eventHandler finalState