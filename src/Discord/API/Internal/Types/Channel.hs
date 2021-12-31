module Discord.API.Internal.Types.Channel where

import Discord.API.Internal.Types.Common (Snowflake)
import Data.Text (Text)
import Data.Aeson.Types (Parser, FromJSON (parseJSON))
import Discord.API.Internal.Types.User (User)
import Data.Function (on)
import Data.Aeson
    ( (.!=), (.:), (.:?), withObject, FromJSON(parseJSON) )


{- 

Channel fields:

- channelId:               the id of this channel
- channelGuildId:          the id of the guild (may be missing for some channel objects received over gateway guild dispatches)
- channelPosition:         sorting position of the channel
- channelName:             the name of the channel (1-100 characters)
- channelTopic:            channel topic (0-1024 characters)
- channelIsNSFW:           whether the channel is NSFW
- channelLastMessageId:    the id of the last message sent in this channel (may not point to an existing or valid message)
- channelBitRate:          the bitrate (in bits) of the voice channel
- channelUserLimit:        the user limit of the voice channel
- channelRateLimitPerUser: amount of seconds a user has to wait before sending another message (0-21600); bots, as well as users with the permission `manage_messages` or `manage_channel`, are unaffected
- channelRecipients:       the recipients of the DM
- channelParentId:         for guild channels: id of the parent category for a channel (each parent category can contain up to 50 channels), for threads: id of the text channel this thread was created 

-}

data Channel 
    = GuildTextChannel                                 -- a text channel within a server
        { channelId               :: Snowflake
        , channelGuildId          :: Maybe Snowflake
        , channelName             :: Text
        , channelPosition         :: Integer
        , channelRateLimitPerUser :: Integer
        , channelIsNSFW           :: Bool
        , channelTopic            :: Text
        , channelLastMessageId    :: Maybe Snowflake
        , channelParentId         :: Maybe Snowflake }
    | GuildVoiceChannel                                -- a voice channel within a server
        { channelId        :: Snowflake
        , channelGuildId   :: Maybe Snowflake
        , channelName      :: Text 
        , channelPosition  :: Integer 
        , channelIsNSFW    :: Bool 
        , channelBitRate   :: Integer 
        , channelUserLimit :: Integer 
        , channelParentId  :: Maybe Snowflake }
    | GuildCategoryChannel                             -- an organizational category that contains up to 50 channels 
        { channelId       :: Snowflake
        , channelGuildId  :: Maybe Snowflake
        , channelName     :: Text 
        , channelPosition :: Integer }
    | GuildNewsChannel                                 -- a channel that users can follow and cross-post into their own server
        { channelId            :: Snowflake
        , channelGuildId       :: Maybe Snowflake
        , channelName          :: Text 
        , channelPosition      :: Integer 
        , channelIsNSFW        :: Bool
        , channelTopic         :: Text
        , channelLastMessageId :: Maybe Snowflake }
    | GuildStoreChannel                                -- a channel in which game developers can sell their game on Discord
        { channelId       :: Snowflake
        , channelGuildId  :: Maybe Snowflake
        , channelName     :: Text 
        , channelPosition :: Integer 
        , channelIsNSFW   :: Bool
        , channelParentId :: Maybe Snowflake }
    | DMChannel                                        -- a direct message between users
        { channelId            :: Snowflake
        , channelRecipients    :: [User]
        , channelLastMessageId :: Maybe Snowflake }
    | GroupDMChannel                                   -- a direct message between multiple users
        { channelId            :: Snowflake
        , channelRecipients    :: [User]
        , channelLastMessageId :: Maybe Snowflake }
    | StageChannel                                     -- a voice channel for hosting events with an audience
        { channelId      :: Snowflake
        , channelGuildId :: Maybe Snowflake
        , channelTopic   :: Text }
    deriving (Show)

instance Eq Channel where
    (==) = (==) `on` channelId

instance FromJSON Channel where
    parseJSON = withObject "Channel" $ \o -> do
        type' <- (o .: "type") :: Parser Int
        case type' of
            0 -> GuildTextChannel <$> o .:  "id"
                                  <*> o .:? "guild_id"
                                  <*> o .:  "name"
                                  <*> o .:  "position"
                                  <*> o .:  "rate_limit_per_user"
                                  <*> o .:? "nsfw" .!= False 
                                  <*> o .:? "topic" .!= ""
                                  <*> o .:? "last_message_id"
                                  <*> o .:? "parent_id"

            1 -> DMChannel <$> o .: "id"
                           <*> o .: "recipients"
                           <*> o .: "last_message_id"

            2 -> GuildVoiceChannel <$> o .:  "id"
                                   <*> o .:? "guild_id"
                                   <*> o .:  "name"
                                   <*> o .:  "position"
                                   <*> o .:? "nsfw" .!= False
                                   <*> o .:  "bitrate"
                                   <*> o .:  "user_limit"
                                   <*> o .:? "parent_id"
            
            3 -> GroupDMChannel <$> o .: "id"
                                <*> o .: "recipients"
                                <*> o .: "last_message_id"

            4 -> GuildCategoryChannel <$> o .:  "id"
                                      <*> o .:? "guild_id"
                                      <*> o .:  "name"
                                      <*> o .:  "position"

            5 -> GuildNewsChannel <$> o .:  "id"
                                  <*> o .:? "guild_id"
                                  <*> o .:  "name"
                                  <*> o .:  "position"
                                  <*> o .:  "nsfw" .!= False 
                                  <*> o .:? "topic" .!= ""
                                  <*> o .:? "last_message_id"
            
            6 -> GuildStoreChannel <$> o .:  "id"
                                   <*> o .:? "guild_id"
                                   <*> o .:  "name"
                                   <*> o .:  "position"
                                   <*> o .:  "nsfw" .!= False
                                   <*> o .:? "parent_id"
            
            13 -> StageChannel <$> o .:  "id"
                               <*> o .:? "guild_id"
                               <*> o .:? "topic" .!= ""
            
            _ -> fail "unknown channel type"