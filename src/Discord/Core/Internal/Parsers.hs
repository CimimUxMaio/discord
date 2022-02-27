{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Discord.Core.Internal.Parsers where
import Control.Applicative (Alternative (empty, (<|>)))
import Discord.API.Internal.Types.BotEvent (BotEvent (MessageCreate))
import Discord.API.Internal.Types.Message (Message (messageText))
import Data.Text (Text)
import qualified Data.Text as Text
import Discord.Core.Context (Context (MessageCtx, CommandCtx))


newtype BotEventParser a = BotEventParser
    { runBotEventParser :: BotEvent -> Maybe a }
    deriving Functor

instance Applicative BotEventParser where
    pure x = BotEventParser (pure . pure $ x)
    BotEventParser f <*> BotEventParser x = BotEventParser $ \e -> f e <*> x e

instance Alternative BotEventParser where
    empty = BotEventParser $ const Nothing
    BotEventParser f <|> BotEventParser g = BotEventParser $ \e -> f e <|> g e

instance Monad BotEventParser where
    return = pure
    BotEventParser x >>= f = BotEventParser $ \e -> x e >>= flip runBotEventParser e . f

instance MonadFail BotEventParser where
    fail _ = empty



type ContextParser = BotEventParser Context

messageCreateParser :: ContextParser
messageCreateParser = BotEventParser $ \case
        MessageCreate msg -> pure $ MessageCtx msg 
        _                 -> fail "not text message"


-- plainTextParser :: Text -> BotEventParser Message
-- plainTextParser commandsPrefix = do
--     MessageCtx msg <- messageCreateParser
--     if commandsPrefix `Text.isPrefixOf` messageText msg
--         then fail "this is a command"
--         else pure msg


commandParser :: Text -> Text -> ContextParser
commandParser prefix' name = do
    MessageCtx msg <- messageCreateParser
    let txt = messageText msg
    case Text.words txt of
        (w:ws) | w == prefix' <> name
            -> pure $ CommandCtx name ws msg
        _   -> fail "not that command"