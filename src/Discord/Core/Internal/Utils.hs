{-# LANGUAGE LambdaCase #-}
module Discord.Core.Internal.Utils where

import Discord.Core.Internal.Types ( BotEventParser (BotEventParser), BotAction, BotM (BotM) )
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.RWS (tell)
import Discord.API.Internal.Types.BotEvent (BotEvent(MessageCreate))
import Discord.API.Internal.Types.Message (Message (messageText))


addParser :: BotEventParser (BotAction s ()) -> BotM s ()
addParser parser = tell [parser]


messageCreateParser :: BotEventParser Message
messageCreateParser = BotEventParser $ \case
        MessageCreate msg -> pure msg 
        _                 -> fail "not text message"


plainTextParser :: Text -> BotEventParser Message
plainTextParser commandsPrefix = do
    msg <- messageCreateParser
    if commandsPrefix `Text.isPrefixOf` messageText msg
        then fail "this is a command"
        else pure msg


commandParser :: Text -> Text -> BotEventParser (Message, [Text])
commandParser prefix' name = do
    msg <- messageCreateParser
    let txt = messageText msg
    case Text.words txt of
        (w:ws) | w == prefix' <> name
            -> pure (msg, ws)
        _   -> fail "not that command"