module Discord.Core.Handlers 
( onMessage
, onCommand ) where

import Discord.Core.Internal.Types (BotAction, BotM (BotM), BotConfig (prefix))
import Data.Text (Text)
import Discord.Core.Internal.Utils (addParser)
import Control.Monad.Reader (MonadReader (reader))
import Discord.API.Internal.Types.Message (Message)
import Discord.Core.Context (Context (CommandCtx, MessageCtx))
import Discord.Core.Internal.Parsers
    ( commandParser, messageCreateParser )


-- | Used for defining how to handle "MessageCreate" events.
onMessage 
    :: (Message -> BotAction s ())  -- ^ Handler function
    -> BotM s ()
onMessage f = addParser $ do
    ctx@(MessageCtx msg) <- messageCreateParser
    pure (ctx, f msg)


-- | Used for defining how to handle a given command.
onCommand 
    :: Text                                   -- ^ Command name
    -> (Message -> [Text] -> BotAction s ())  -- ^ Handler function
    -> BotM s ()
onCommand name f = do
    prefix' <- reader prefix
    addParser $ do
        ctx@(CommandCtx _ params msg) <- commandParser prefix' name
        pure (ctx, f msg params)