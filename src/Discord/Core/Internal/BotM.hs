{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}

module Discord.Core.Internal.BotM where

import Discord.Core.Internal.Types (BotAction, BotM (_runBotM), BotHandlerEventParser, BotApp (..), BotEventParser (runBotEventParser), BotConfig)
import Discord.API.Internal.Types.BotEvent (BotEvent)
import Discord.Core.Context (Context (NoCtx))
import Control.Monad.Trans.Writer (runWriter)
import Data.Maybe (fromMaybe)
import Data.Foldable (asum)
import Control.Monad.Trans.Reader (runReaderT)


runBotM :: BotConfig -> BotM s a -> [BotHandlerEventParser s]
runBotM cfg = snd . runWriter . (`runReaderT` cfg) . _runBotM


appEventHandler :: BotApp s -> BotEvent -> (Context, BotAction s ())
appEventHandler BotApp{appConfig, appDefinition} = eventHandler appConfig appDefinition


-- Used for testing
eventHandler :: BotConfig -> BotM s () -> BotEvent -> (Context, BotAction s ())
eventHandler cfg m event =
    fromMaybe default' . ($ event) . runBotEventParser . asum $ parsers
    where 
        default' = (NoCtx, pure ())
        parsers = runBotM cfg m