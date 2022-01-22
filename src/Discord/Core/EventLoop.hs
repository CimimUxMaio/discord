{-# LANGUAGE RankNTypes #-}
module Discord.Core.EventLoop (startEventLoop) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent (readChan, Chan)
import Discord.API.Internal.Types.BotEvent (BotEvent)
import Discord.Core.Internal.Types (BotApp(BotApp, appConfig, appInitialState), runBotAction, botAppEventHandler)
import Control.Exception (catch, SomeException)



startEventLoop :: Chan BotEvent -> BotApp s -> IO ()
startEventLoop eventQueue app = loop eventQueue eventHandler initialState
    where eventHandler currentState event = snd <$> (runBotAction cfg currentState . botAppEventHandler app $ event)
          cfg = appConfig app
          initialState = appInitialState app


loop :: Chan BotEvent -> (s -> BotEvent -> IO s) -> s -> IO ()
loop eventQueue eventHandler currentState = do
    event <- readChan eventQueue
    finalState <- catch (eventHandler currentState event) (handleException currentState)
    loop eventQueue eventHandler finalState


handleException :: s -> SomeException -> IO s  -- Handles user produced exceptions
handleException prevState e = print e >> pure prevState