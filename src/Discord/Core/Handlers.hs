module Discord.Core.Handlers 
( onMessage
, onCommand ) where

import Discord.Core.Internal.Types (BotAction, BotM (BotM), BotConfig (prefix))
import Data.Text (Text)
import Discord.Core.Internal.Utils (addParser)
import Control.Monad.Reader (MonadReader (reader))
import Discord.API.Internal.Types.Message (Message)
import Discord.Core.Internal.Parsers
    ( BotEventParser, messageCreateParser, commandParser )
import Discord.Core.Context (Context (CommandCtx))


onMessage :: (Message -> BotAction s ()) -> BotM s ()
onMessage = undefined -- (messageCreateParser #>)


onCommand :: Text -> (Message -> [Text] -> BotAction s ()) -> BotM s ()
onCommand name f = do
    prefix' <- reader prefix
    addParser $ do
        ctx@(CommandCtx _ params msg) <- commandParser prefix' name
        pure (ctx, f msg params)