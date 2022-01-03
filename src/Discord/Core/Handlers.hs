module Discord.Core.Handlers where
import Discord.Core.Internal.Types (BotAction, BotM (BotM), BotEventParser, BotConfig (prefix))
import Data.Text (Text)
import Discord.Core.Internal.Utils (addParser, messageCreateParser, commandParser)
import Control.Monad.Reader (MonadTrans(lift), MonadReader (reader))
import Discord.API.Internal.Types.Message (Message)



(#>) :: BotEventParser a -> (a -> BotAction ()) -> BotM ()
(#>) p f = addParser $ f <$> p


onMessage :: (Message -> BotAction ()) -> BotM ()
onMessage = (messageCreateParser #>)


onCommand :: Text -> ((Message, [Text]) -> BotAction ()) -> BotM ()
onCommand name f = do
    prefix' <- reader prefix
    commandParser prefix' name #> f