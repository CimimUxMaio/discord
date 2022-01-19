{-# LANGUAGE RecordWildCards #-}
module Discord.API.Internal.Http.Types where
import Data.Text (Text)
import Discord.API.Internal.Types.Embed (Embed)
import Discord.API.Internal.Types.Common (Snowflake)
import Data.Aeson (ToJSON (toJSON), object, (.=))


data SendableMessage = SendableText     Text
                     | SendableEmbeds   [Embed]
                     | SendableStickers [Snowflake]
                     deriving Show

instance ToJSON SendableMessage where
    toJSON (SendableText txt) = 
        object [ "content" .= txt ]
    
    toJSON (SendableEmbeds embeds) =
        object [ "embeds" .= embeds ]

    toJSON (SendableStickers stickerIds) = 
        object [ "sticker_ids" .= stickerIds ]