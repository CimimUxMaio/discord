module Discord.Core.Comms where
import Discord.Core.Internal.Types (BotConfig (token), BotAction)
import Data.Text (Text)
import Control.Monad.RWS (asks, MonadIO (liftIO))
import Discord.API.Internal.Http.Channel (sendMessage)
import Discord.API.Internal.Types.Message (Message)
import Discord.API.Internal.Http.Types (SendableMessage (SendableText, SendableEmbeds, SendableStickers))
import Discord.API.Internal.Types.Common (Snowflake)
import Discord.API.Internal.Types.Embed (Embed)


liftWithToken :: (Text -> IO a) -> BotAction a
liftWithToken f = asks token >>= (liftIO . f)


send :: Snowflake -> SendableMessage -> BotAction Message
send chid = liftWithToken . sendMessage chid


sendText :: Snowflake -> Text -> BotAction Message
sendText chid = send chid . SendableText


sendEmbeds :: Snowflake -> [Embed] -> BotAction Message
sendEmbeds chid = send chid . SendableEmbeds


sendStickers :: Snowflake -> [Snowflake] -> BotAction Message 
sendStickers chid = send chid . SendableStickers