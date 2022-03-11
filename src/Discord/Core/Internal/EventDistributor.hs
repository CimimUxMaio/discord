{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Discord.Core.Internal.EventDistributor (eventDistributor) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent (readChan, Chan, forkIO)
import Discord.API.Internal.Types.BotEvent (BotEvent)
import Control.Exception (catch, SomeException (SomeException), catches, Handler (Handler))
import Control.Monad.RWS (MonadState(put, get), void, forever)
import Control.Monad.Trans.State (StateT(runStateT))
import Discord.Core.Internal.Utils
    ( toHandler )
import Data.Bifunctor (second)
import Discord.Core.Context (Context)
import Control.Concurrent.STM (newTVarIO, putTMVar, readTVarIO, atomically, swapTMVar, isEmptyTMVar, tryPutTMVar)
import Data.List (find)
import Discord.Core.Internal.BotM (appEventHandler)
import Discord.Core.Internal.Types
    ( BotEnv(..), WaitingTask(..), BotApp(..), BotExceptionHandler (BotExceptionHandler) )
import Discord.Core.Internal.BotAction (runBotAction_)
import GHC.Conc (STM(STM))
import Control.Monad (unless)


eventDistributor :: Chan BotEvent -> BotApp s -> IO ()
eventDistributor eventQueue app@BotApp{..} = do
    state <- newTVarIO appInitialState
    waitingTasks <- newTVarIO []

    let botEnv = BotEnv { envConfig        = appConfig
                        , envState         = state
                        , _envWaitingTasks = waitingTasks
                        }

    eventLoop eventQueue app botEnv



eventLoop :: Chan BotEvent -> BotApp s -> BotEnv s -> IO ()
eventLoop eventQueue app env@BotEnv{_envWaitingTasks} = forever $ do
    event <- readChan eventQueue

    waitingTasks <- readTVarIO _envWaitingTasks

    case find (`wtMatches` event) waitingTasks of
        Nothing          -> createNewTask event env
        Just waitingTask -> resumeWith waitingTask event

    where
        --createNewTask :: BotEvent -> BotEnv s -> IO ()
        createNewTask event env = do
            let (ctx, action) = appEventHandler app event
            let run = catches (runBotAction_ env action) (exceptionHandlers app env ctx)
            void $ forkIO run

        resumeWith :: WaitingTask -> BotEvent -> IO ()
        resumeWith WaitingTask{wtContainer} event = void . atomically $ do
            success <- tryPutTMVar wtContainer (Just event)
            unless success . void $ swapTMVar wtContainer (Just event)

        defaultExceptionHandler :: BotExceptionHandler s
        defaultExceptionHandler = BotExceptionHandler $ \_ (SomeException e) -> liftIO $ print e

        --exceptionHandlers :: BotApp s -> BotEnv s -> Context -> [Handler s]
        exceptionHandlers app env taskContext =
            map (toHandler env taskContext) (appExceptionHandlers app ++ [defaultExceptionHandler])


