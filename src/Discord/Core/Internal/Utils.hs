module Discord.Core.Internal.Utils where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.RWS (tell)
import Discord.API.Internal.Types.BotEvent (BotEvent(MessageCreate))
import Discord.API.Internal.Types.Message (Message (messageText))
import Control.Exception (Handler (Handler))
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Discord.Core.Context (Context (NoCtx))
import Discord.Core.Internal.Types
    ( BotAction,
      BotEnv,
      BotM,
      BotExceptionHandler(..),
      BotEventParser )
import Discord.Core.Internal.BotAction (runBotAction_)


addParser :: BotEventParser (Context, BotAction s ()) -> BotM s ()
addParser parser = tell [parser]


toHandler :: BotEnv s -> Context -> BotExceptionHandler s -> Handler ()
toHandler env ctx (BotExceptionHandler h) = Handler $ runBotAction_ env . h ctx