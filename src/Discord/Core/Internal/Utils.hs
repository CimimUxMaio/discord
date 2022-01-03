module Discord.Core.Internal.Utils where

import Discord.Core.Internal.Types ( BotEventParser, BotAction, BotM (BotM) )
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.RWS (tell)



addParser :: BotEventParser (BotAction ()) -> BotM ()
addParser parser = tell [parser]


textParser :: BotEventParser Text
textParser = undefined


plainTextParser :: Text -> BotEventParser Text
plainTextParser commandsPrefix = do
    txt <- textParser
    if commandsPrefix `Text.isPrefixOf` txt
        then fail "this is a command"
        else pure txt


commandParser :: Text -> Text -> BotEventParser Text
commandParser prefix' name = do
    txt <- textParser
    case Text.words txt of
        (w:ws) | w == prefix' <> name
            -> pure $ Text.unwords ws
        _   -> fail "not that command"