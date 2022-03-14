{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
module Discord.API.Internal.Types.Guild where

import Discord.API.Internal.Types.Common ( ImageHash, Snowflake(..) )
import Data.Text (Text)
import Discord.API.Internal.Types.User (User (userId))
import Data.Function (on)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?), (.!=), object, ToJSON (toJSON), (.=))
import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime)
import Data.Aeson.Types (ToJSON)


newtype GuildUnavailable = 
    GuildUnavailable Snowflake deriving (Show, Eq)

instance FromJSON GuildUnavailable where
    parseJSON = withObject "GuildUnavailable" $ \o ->
        GuildUnavailable <$> o .: "id"


data Guild = Guild
    { guildId                          :: Snowflake        -- guild id
    , guildName                        :: Text             -- guild name (2-100 characters, excluding trailing and leading whitespace)
    , guildDescription                 :: Maybe Text       -- the description of a Community guild 
    , guildIcon                        :: Maybe ImageHash  -- icon hash
    , guildSplash                      :: Maybe ImageHash  -- splash hash 
    , guildOwnerId                     :: Snowflake        -- id of the guild's owner
    , guildAFKChannelId                :: Maybe Snowflake  -- id of AFK channel
    , guildAFKTimeout                  :: Integer          -- AFK timeout in seconds
    , guildPublicUpdatesChannelId      :: Maybe Snowflake  -- the id of the channel where admins and moderators of Community guilds receive notices from Discord
    , guildRoles                       :: [Role]           -- roles in the guild
    , guildEmojis                      :: [Emoji]          -- custom guild emojis
    , guildStickers                    :: [Sticker]        -- custom guild stickers
    } deriving (Show)

instance Eq Guild where
    (==) = (==) `on` guildId

instance FromJSON Guild where
    parseJSON = withObject "Guild" $ \o ->
        Guild <$> o .:  "id"
              <*> o .:  "name"
              <*> o .:  "description"
              <*> o .:? "icon"
              <*> o .:? "splash"
              <*> o .:  "owner_id"
              <*> o .:? "afk_channel_id"
              <*> o .:  "afk_timeout"
              <*> o .:? "public_updates_channel_id"
              <*> o .:  "roles"
              <*> o .:  "emojis"
              <*> o .:? "stickers" .!= []


data Emoji = Emoji
    { emojiId         :: Maybe Snowflake    -- emoji id
    , emojiName       :: Maybe Text         -- emoji name
    , emojiRoleIds    :: [Snowflake]        -- roles allowed to use this emoji
    , emojiCreator    :: Maybe User         -- user that created this emoji
    , emojiIsManaged  :: Bool               -- whether this emoji is managed
    , emojiIsAnimated :: Bool               -- whether this emoji is animated
    } deriving Show

instance Eq Emoji where
    (==) = (==) `on` emojiId

instance FromJSON Emoji where
    parseJSON = withObject "Emoji" $ \o ->
        Emoji <$> o .:? "id"
              <*> o .:? "name"
              <*> o .:? "roles" .!= []
              <*> o .:? "user"
              <*> o .:? "managed"  .!= False
              <*> o .:? "animated" .!= False

instance ToJSON Emoji where
    toJSON Emoji{..} = object [ "id"   .= emojiId
                              , "name" .= emojiName 
                              ]


data Sticker = Sticker
    { stickerId      :: Snowflake
    , stickerName    :: Text 
    , stickerGuildId :: Maybe Snowflake
    , stickerCreator :: Maybe User
    } deriving (Show)

instance Eq Sticker where
    (==) = (==) `on` stickerId

instance FromJSON Sticker where
    parseJSON = withObject "Sticker" $ \o ->
        Sticker <$> o .:  "id"
                <*> o .:  "name"
                <*> o .:? "guild_id"
                <*> o .:? "user"


data Role = Role
    { roleId            :: Snowflake       -- role id
    , roleName          :: Text            -- role name
    , roleColor         :: Integer         -- role color
    , roleHoist         :: Bool            -- if this role is pinned in the user listing
    , roleIcon          :: Maybe ImageHash -- role icon hash
    , rolePosition      :: Integer         -- position of this role
    , rolePermissions   :: Integer         -- permission bit set
    , roleIsManaged     :: Bool            -- whether this role is managed by an integration
    , roleIsMentionable :: Bool            -- whether this role is mentionable
    } deriving (Show)

instance Eq Role where
    (==) = (==) `on` roleId

instance FromJSON Role where
    parseJSON = withObject "Role" $ \o ->
        Role <$> o .:  "id"
             <*> o .:  "name"
             <*> o .:  "color"
             <*> o .:  "hoist"
             <*> o .:? "icon"
             <*> o .:  "position"
             <*> o .:  "permissions"
             <*> o .:  "managed"
             <*> o .:  "mentionable"



data GuildMember = GuildMember
    { guildMemberUser        :: Maybe User      -- the user this guild member represents (not included in MESSAGE_CREATE and MESSAGE_UPDATE gateway events)
    , guildMemberNick        :: Maybe Text      -- the user's guild nickname
    , guildMemberAvatar      :: Maybe ImageHash -- the member's guild avatar hash
    , guildMemberRoleIds     :: [Snowflake]     -- array of role object ids
    , guildMemberJoinedAt    :: UTCTime         -- when the user joined the guild
    , guildMemberIsDeaf      :: Bool            -- whether the user is deafened in voice channels
    , guildMemberIsMuted     :: Bool            -- whether the user is muted in voice channels
    , guildMemberPermissions :: Maybe Text      -- total permissions of the member in the channel, including overwrites, returned when in the interaction object
    } deriving Show

instance FromJSON GuildMember where
    parseJSON = withObject "GuildMember" $ \o ->
        GuildMember <$> o .:? "user"
                    <*> o .:? "nick"
                    <*> o .:? "avatar"
                    <*> o .:  "roles"
                    <*> o .:  "joined_at"
                    <*> o .:  "deaf"
                    <*> o .:  "mute"
                    <*> o .:? "permissions"