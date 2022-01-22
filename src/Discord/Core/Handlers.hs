module Discord.Core.Handlers where
import Discord.Core.Internal.Types (BotAction, BotM (BotM), BotEventParser, BotConfig (prefix))
import Data.Text (Text)
import Discord.Core.Internal.Utils (addParser, messageCreateParser, commandParser)
import Control.Monad.Reader (MonadReader (reader))
import Discord.API.Internal.Types.Message (Message)


(#>) :: BotEventParser a -> (a -> BotAction s ()) -> BotM s ()
(#>) p f = addParser $ f <$> p


onMessage :: (Message -> BotAction s ()) -> BotM s ()
onMessage = (messageCreateParser #>)


onCommand :: Text -> ((Message, [Text]) -> BotAction s ()) -> BotM s ()
onCommand name f = do
    prefix' <- reader prefix
    commandParser prefix' name #> f