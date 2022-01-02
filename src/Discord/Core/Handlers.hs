module Discord.Core.Handlers where
import Discord.Core.Internal.Types (BotAction, BotM (BotM), BotEventParser, BotConfig (prefix))
import Data.Text (Text)
import Discord.Core.Internal.Utils (addParser, textParser, commandParser)
import Control.Monad.Reader (MonadTrans(lift), MonadReader (reader))



(#>) :: BotEventParser a -> (a -> BotAction ()) -> BotM ()
(#>) p f = addParser $ f <$> p


onMessage :: (Text -> BotAction ()) -> BotM ()
onMessage = (textParser #>)


onCommand :: Text -> (Text -> BotAction ()) -> BotM ()
onCommand name f = do
    prefix' <- reader prefix
    commandParser prefix' name #> f