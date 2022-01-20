module Discord.API.Internal.Http.Cdn where
import Data.Text (Text, pack, unpack)
import Discord.API.Internal.Http.Common ((/+))


cdnBaseUrl :: Text
cdnBaseUrl = "https://cdn.discordapp.com"


cdnUserAvatarUrl :: Text -> Text -> Text
cdnUserAvatarUrl uid hash = cdnBaseUrl /+ "avatars" /+ uid /+ hash <> ".png"


cdnUserDefaultAvatarUrl :: Text -> Text
cdnUserDefaultAvatarUrl userDisc = cdnBaseUrl /+ "embed" /+ "avatars" /+ defaultIndex <> ".png"
    where defaultIndex = pack . show .(`mod` 5) . read . unpack $ userDisc