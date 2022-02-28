{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Discord.Core.Internal.EventLoop (startEventLoop) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent (readChan, Chan)
import Discord.API.Internal.Types.BotEvent (BotEvent)
import Control.Exception (catch, SomeException (SomeException), catches, Handler (Handler))
import Control.Monad.RWS (MonadState(put, get), void, forever)
import Control.Monad.Trans.State (StateT(runStateT))
import Discord.Core.Internal.Types (BotApp (appConfig, appInitialState, appExceptionHandlers), runBotAction, runBotAction_, BotConfig)
import Discord.Core.Internal.Utils
    ( toHandler, botAppEventHandler )
import Data.Bifunctor (second)
import Discord.Core.Context (Context)


startEventLoop :: Chan BotEvent -> BotApp s -> IO ()
startEventLoop eventQueue app = void . (`runStateT` initialState) . forever $ do
    event <- liftIO $ readChan eventQueue
    currentState <- get
    let cfg = appConfig app
    let (ctx, action) = botAppEventHandler app event
    let runEvent = runBotAction_ cfg currentState action
    finalState <- liftIO $ catches runEvent (exceptionHandlers app currentState ctx)
    put finalState

    where initialState = appInitialState app


defaultExceptionHandler :: s -> Handler s
defaultExceptionHandler prevState = 
    Handler $ \(e :: SomeException) -> print e >> pure prevState


exceptionHandlers :: BotApp s -> s -> Context -> [Handler s]
exceptionHandlers app currentState currentCtx = 
    userExceptionHandlers ++ [defaultExceptionHandler currentState]
    where userExceptionHandlers = map (toHandler cfg currentState currentCtx) (appExceptionHandlers app)
          cfg = appConfig app