module Discord.Core.Handlers where
import Discord.Core.Internal.Types (BotAction, BotM (BotM), BotEventParser, BotConfig (prefix))
import Data.Text (Text)
import Discord.Core.Internal.Utils (addParser, textParser, commandParser)
import Control.Monad.Trans.Reader (reader)
import Control.Monad.Reader (MonadTrans(lift))



(#>) :: BotEventParser a -> (a -> BotAction ()) -> BotM ()
(#>) p f = addParser $ f <$> p


onMessage :: (Text -> BotAction ()) -> BotM ()
onMessage = (textParser #>)

onCommand :: Text -> (Text -> BotAction ()) -> BotM ()
onCommand name f = do
    prefix' <- BotM $ reader prefix
    commandParser prefix' name #> f