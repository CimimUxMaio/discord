module Discord.Core.Comms where
import Discord.Core.Internal.Types (BotConfig (token), BotAction)
import Data.Text (Text)
import Control.Monad.RWS (asks, MonadIO (liftIO))
import Discord.API.Internal.Http.Channel (sendMessage)
import Discord.API.Internal.Types.Message (Message)
import Discord.API.Internal.Http.Types (SendableMessage (SendableText, SendableEmbeds, SendableStickers))
import Discord.API.Internal.Types.Common (Snowflake)
import Discord.API.Internal.Types.Embed (Embed)


liftWithToken :: (Text -> IO a) -> BotAction s a
liftWithToken f = asks token >>= (liftIO . f)


-- | Sends a sendable value to the given channel id.
send 
    :: Snowflake            -- ^ Text channel ID
    -> SendableMessage      -- ^ Sendable
    -> BotAction s Message  -- ^ Action
send chid = liftWithToken . sendMessage chid


{- | @send@ wrapper.
Sends text to the given channel id.
-}
sendText 
    :: Snowflake            -- ^ Text channel ID
    -> Text                 -- ^ Text to be sent
    -> BotAction s Message  -- ^ Action
sendText chid = send chid . SendableText


{- | @send@ wrapper.
Sends embeds to the given channel id.
-}
sendEmbeds 
    :: Snowflake            -- ^ Text channel ID
    -> [Embed]              -- ^ Embed list
    -> BotAction s Message  -- ^ Action
sendEmbeds chid = send chid . SendableEmbeds


{- | @send@ wrapper.
Sends stickers to the given channel id.
-}
sendStickers 
    :: Snowflake            -- ^ Text channel ID
    -> [Snowflake]          -- ^ Sticker list
    -> BotAction s Message  -- ^ Action
sendStickers chid = send chid . SendableStickers