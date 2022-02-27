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


addParser :: BotEventParser (BotAction s ()) -> BotM s ()
addParser parser = tell [parser]

toHandler :: BotConfig -> s -> BotExceptionHandler s -> Handler s
toHandler cfg state (BotExceptionHandler h) = Handler $ (snd <$>) . runBotAction cfg state . h

botAppEventHandler :: BotApp s -> BotEvent -> BotAction s ()
botAppEventHandler app event =
    fromMaybe (pure ()) . ($ event) . runBotEventParser . asum $ runBotM (appHandler app) (appConfig app)