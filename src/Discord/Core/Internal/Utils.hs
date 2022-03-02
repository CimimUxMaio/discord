module Discord.Core.Internal.Utils where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.RWS (tell)
import Discord.API.Internal.Types.BotEvent (BotEvent(MessageCreate))
import Discord.API.Internal.Types.Message (Message (messageText))
import Discord.Core.Internal.Parsers (BotEventParser (runBotEventParser))
import Discord.Core.Internal.Types (BotAction, BotM, BotExceptionHandler (BotExceptionHandler), BotConfig, BotApp (appHandler, appConfig), runBotAction, runBotM)
import Control.Exception (Handler (Handler))
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Discord.Core.Context (Context (NoCtx))


addParser :: BotEventParser (Context, BotAction s ()) -> BotM s ()
addParser parser = tell [parser]


toHandler :: BotConfig -> s -> Context -> BotExceptionHandler s -> Handler s
toHandler cfg state ctx (BotExceptionHandler h) = Handler action
    where action = (snd <$>) . runBotAction cfg state . h ctx


eventHandler :: BotConfig -> BotM s () -> BotEvent -> (Context, BotAction s ())
eventHandler cfg m event =
    fromMaybe default' . ($ event) . runBotEventParser . asum $ runBotM m cfg
    where default' = (NoCtx, pure ())


botAppEventHandler :: BotApp s -> BotEvent -> (Context, BotAction s ())
botAppEventHandler app = eventHandler (appConfig app) (appHandler app)