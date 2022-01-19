{-# LANGUAGE RecordWildCards #-}
module Discord.API.Internal.Http.Types where
import Data.Text (Text)
import Discord.API.Internal.Types.Embed (Embed)
import Discord.API.Internal.Types.Common (Snowflake)
import Data.Aeson (ToJSON (toJSON), object, (.=))


data Sendable = 
    Sendable
        { sendableText       :: Text
        , sendableEmbeds     :: [Embed]
        , sendableStickerIds :: [Snowflake]
        }
    deriving Show

instance ToJSON Sendable where
    toJSON Sendable{..} = 
        object [ "content"     .= sendableText
               , "embeds"      .= sendableEmbeds
               , "sticker_ids" .= sendableStickerIds
               ]