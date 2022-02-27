{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Discord.Core.Internal.EventLoop (startEventLoop) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent (readChan, Chan)
import Discord.API.Internal.Types.BotEvent (BotEvent)
import Control.Exception (catch, SomeException (SomeException), catches, Handler (Handler))
import Control.Monad.RWS (MonadState(put, get), void, forever)
import Control.Monad.Trans.State (StateT(runStateT))
import Discord.Core.Internal.Types (BotApp (appConfig, appInitialState, appExceptionHandlers), runBotAction)
import Discord.Core.Internal.Utils
    ( toHandler, botAppEventHandler )


startEventLoop :: Chan BotEvent -> BotApp s -> IO ()
startEventLoop eventQueue app = void . (`runStateT` initialState) . forever $ do
    event <- liftIO $ readChan eventQueue
    currentState <- get
    finalState <- liftIO $ catches (eventHandler currentState event) (exceptionHandlers currentState)
    put finalState

    where eventHandler currentState = (snd <$>) . runBotAction cfg currentState . botAppEventHandler app
          cfg = appConfig app
          initialState = appInitialState app
          userExceptionHandlers currentState = map (toHandler cfg currentState) (appExceptionHandlers app)
          exceptionHandlers currentState = userExceptionHandlers currentState ++ [defaultExceptionHandler currentState]


defaultExceptionHandler :: s -> Handler s
defaultExceptionHandler prevState = 
    Handler $ \(e :: SomeException) -> print e >> pure prevState