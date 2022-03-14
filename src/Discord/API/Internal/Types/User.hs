{-# LANGUAGE RecordWildCards #-}
module Discord.API.Internal.Types.User where

import Discord.API.Internal.Types.Common (Snowflake (Snowflake), ImageHash (ImageHash))
import Data.Text (Text, pack, unpack)
import Data.Aeson (FromJSON, withObject, (.:), (.:?), (.!=), ToJSON (toJSON), object, (.=))
import Data.Aeson.Types (FromJSON(parseJSON), Parser)
import Data.Function (on)
import Discord.API.Internal.Http.Cdn (cdnUserDefaultAvatarUrl, cdnUserAvatarUrl)


data User = User
    { userId            :: Snowflake         -- the user's ID
    , userName          :: Text              -- the user's username (not unique)
    , userDiscriminator :: Text              -- the user's 4-digit discord-tag
    , userAvatarUrl     :: Text              -- the user's avatar hash [NULLABLE]
    , userIsBot         :: Bool              -- whether the user belongs to an OAuth2 application [OPTIONAL]
    -- userIsSystem                          -- whether the user is an Official Discord System user (part of the urgent message system) [OPTIONAL]  
    , userMFA           :: Maybe Bool        -- whether the user has two factor enabled on their account [OPTIONAL]
    -- userBanner                            -- the user's banner hash [OPTIONAL & NULLABLE]
    -- userBannerColor                       -- the user's banner color encoded as an integer representation of hexadecimal color code [OPTIONAL & NULLABLE]
    -- userLanguage                          -- the user's chosen language [OPTIONAL]
    -- userIsVerified                        -- whether the email on this account has been verified [OPTIONAL]
    , userEmail         :: Maybe Text        -- the user's email [OPTIONAL & NULLABLE]
    -- userFlags                             -- the flags on a user's account [OPTIONAL]
    -- userPremiumType                       -- the type of Nitro subscription on a user's account [OPTIONAL]
    -- userPublicFlags                       -- the public flags on a user's account [OPTIONAL]
    } deriving (Show)

instance Eq User where
    (==) = (==) `on` userId

instance FromJSON User where
    parseJSON = withObject "User" $ \o ->
        User <$> o .:  "id"
             <*> o .:  "username"
             <*> o .:  "discriminator"
             <*> makeAvatarUrl o
             <*> o .:? "bot" .!= False
             <*> o .:? "mfa_enabled"
             <*> o .:? "email"
        
        where makeAvatarUrl o = do
                uid        <- o .: "id"            :: Parser Text
                disc       <- o .: "discriminator" :: Parser Text
                avatarHash <- o .:? "avatar"       :: Parser (Maybe Text)
                pure (maybe (cdnUserDefaultAvatarUrl disc) (cdnUserAvatarUrl uid) avatarHash)


instance ToJSON User where 
    toJSON User{..}= object [ "id"            .= userId
                            , "username"      .= userName
                            , "discriminator" .= userDiscriminator 
                            ]        