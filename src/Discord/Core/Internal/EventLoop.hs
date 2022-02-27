{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Discord.Core.Internal.EventLoop (startEventLoop) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent (readChan, Chan)
import Discord.API.Internal.Types.BotEvent (BotEvent)
import Control.Exception (catch, SomeException (SomeException), catches, Handler (Handler))
import Control.Monad.RWS (MonadState(put, get), void, forever)
import Control.Monad.Trans.State (StateT(runStateT))
import Discord.Core.Internal.Types (BotApp (appConfig, appInitialState, appExceptionHandlers), runBotAction, runBotAction_)
import Discord.Core.Internal.Utils
    ( toHandler, botAppEventHandler )
import Data.Bifunctor (second)


startEventLoop :: Chan BotEvent -> BotApp s -> IO ()
startEventLoop eventQueue app = void . (`runStateT` initialState) . forever $ do
    event <- liftIO $ readChan eventQueue
    currentState <- get
    let (ctx, action) = botAppEventHandler app event
    finalState <- liftIO $ catches (runBotAction_ cfg currentState action) (exceptionHandlers currentState ctx)
    put finalState

    where eventHandler = botAppEventHandler app
          cfg = appConfig app
          initialState = appInitialState app
          userExceptionHandlers currentState currentCtx = map (toHandler cfg currentState currentCtx) (appExceptionHandlers app)
          exceptionHandlers currentState currentCtx = 
              userExceptionHandlers currentState currentCtx ++ [defaultExceptionHandler currentState]


defaultExceptionHandler :: s -> Handler s
defaultExceptionHandler prevState = 
    Handler $ \(e :: SomeException) -> print e >> pure prevState