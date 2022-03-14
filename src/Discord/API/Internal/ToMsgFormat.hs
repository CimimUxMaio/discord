{-# LANGUAGE RecordWildCards #-}
module Discord.API.Internal.ToMsgFormat where
import Data.Text (Text)
import Discord.API.Internal.Types.Guild (Emoji (..))
import Data.Maybe (fromMaybe)
import Discord.API.Internal.Types.Common (Snowflake (Snowflake))



class ToMsgFormat a where
    toMsgFormat :: a -> Text


instance ToMsgFormat Emoji where
    toMsgFormat Emoji{..} = maybe name customEmoji emojiId        
        where
            customEmoji (Snowflake eid) = "<" <> if emojiIsAnimated then "a" else "" <> ":" <> name <> ":" <> eid <> ">"
            name = fromMaybe "" emojiName