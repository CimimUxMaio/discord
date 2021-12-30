module Discord.API.Types where
import Data.Text (Text)
import Data.Word (Word64)
import Data.Function (on)
import Data.Maybe (fromJust)
import Data.List.NonEmpty (NonEmpty)



data BotEvent = Ready GatewayVersion User [GuildUnavailable] SessionID
              | ChannelCreate Channel
              | ChannelUpdate Channel
              | ChannelDelete Channel
              | ChannelPinsUpdate Channel
              deriving (Show)

type SessionID = Text
newtype GatewayVersion = GatewayVersion Integer deriving (Show, Eq)


newtype Snowflake = Snowflake Word64 deriving (Show, Eq)


newtype ImageHash = ImageHash Text deriving (Show, Eq)
newtype ColorInt = ColorInt Integer deriving (Show, Eq)
newtype Language = Language Text deriving (Show, Eq)


data PremiumType = None         -- 0
                 | NitroClassic -- 1
                 | Nitro        -- 2
                 deriving (Show, Eq, Enum)

data User = User
    { userId            :: Snowflake         -- the user's ID
    , userName          :: Text              -- the user's username (not unique)
    , userDiscriminator :: Text              -- the user's 4-digit discord-tag
    , userAvatar        :: Maybe ImageHash   -- the user's avatar hash [NULLABLE]
    , userIsBot         :: Maybe Bool        -- whether the user belongs to an OAuth2 application [OPTIONAL]
    -- userIsSystem                          -- whether the user is an Official Discord System user (part of the urgent message system) [OPTIONAL]  
    -- userMFA          :: Maybe Bool        -- whether the user has two factor enabled on their account [OPTIONAL]
    , userBanner        :: Maybe ImageHash   -- the user's banner hash [OPTIONAL & NULLABLE]
    , userBannerColor   :: Maybe ColorInt    -- the user's banner color encoded as an integer representation of hexadecimal color code [OPTIONAL & NULLABLE]
    , userLanguage      :: Maybe Language    -- the user's chosen language [OPTIONAL]
    -- userIsVerified   :: Maybe Bool        -- whether the email on this account has been verified [OPTIONAL]
    , userEmail         :: Maybe Text        -- the user's email [OPTIONAL & NULLABLE]
    -- userFlags        :: Maybe Integer     -- the flags on a user's account [OPTIONAL]
    , userPremiumType   :: Maybe PremiumType -- the type of Nitro subscription on a user's account [OPTIONAL]
    -- userPublicFlags                       -- the public flags on a user's account [OPTIONAL]
    } deriving (Show)

instance Eq User where
    (==) = (==) `on` userId


data ChannelType = GuildText            -- 0  a text channel within a server
                 | DM                   -- 1  a direct message between users
                 | GuildVoice           -- 2  a voice channel within a server
                 | GroupDM              -- 3  a direct message between multiple users
                 | GuildCategory        -- 4  an organizational category that contains up to 50 channels
                 | GuildNews            -- 5  a channel that users can follow and cross-post into their own server
                 | GuildStore           -- 6  a channel in which game developers can sell their game on Discord
                 | GuildNewsThread      -- 10 a temporary sub-channel within a GuildNews channel
                 | GuildPublicThread    -- 11 a temporary sub-channel withing a GuildText channel
                 | GuildPrivateThread   -- 12 a temporary sub-channel within a GuildText channel that is only viewable by those invited and those with `MANAGE_THREADS` permissions
                 | GuildStageVoice      -- 13 a voice channel for hosting events with an audience
                 deriving (Show, Eq)


data Channel = Channel 
    { channelId                   :: Snowflake        -- the id of this channel
    , channelType                 :: ChannelType      -- the type of channel
    , channelGuildId              :: Maybe Snowflake  -- the id of the guild (may be missing for some channel objects received over gateway guild dispatches) [OPTIONAL]
    , channelPosition             :: Maybe Integer    -- sorting position of the channel [OPTIONAL]
    -- channelPermissionOverwrites                    -- explicit permission overwrites for members and roles [OPTIONAL]
    , channelName                 :: Maybe Text       -- the name of the channel (1-100 characters) [OPTIONAL]
    , channelTopic                :: Maybe Text       -- channel topic (0-1024 characters) [OPTIONAL & NULLABLE]
    , channelNSFW                 :: Maybe Bool       -- whether the channel is NSFW [OPTIONAL]
    , channelLastMessageId        :: Maybe Snowflake  -- the id of the last message sent in this channel (may not point to an existing or valid message) [OPTIONAL & NULLABLE]
    -- channelBitrate             :: Maybe Integer    -- the bitrate (in bits) of the voice channel [OPTIONAL]
    , channelUserLimit            :: Maybe Integer    -- the user limit of the voice channel [OPTIONAL]
    , channelRateLimitPerUser     :: Maybe Integer    -- amount of seconds a user has to wait before sending another message (0-21600); bots, as well as users with the permission `manage_messages` or `manage_channel`, are unaffected [OPTIONAL]
    , channelRecipients           :: [User]           -- the recipients of the DM [OPTIONAL]
    , channelIcon                 :: Maybe ImageHash  -- icon hash [OPTIONAL & NULLABLE]
    , channelOwnerId              :: Maybe Snowflake  -- id of the creator of the group DM or thread [OPTIONAL]
    -- channelApplicationId       :: Maybe Snowflake  -- application id of the group DM creator if it is bot-created [OPTIONAL]
    , channelParentId             :: Maybe Snowflake  -- for guild channels: id of the parent category for a channel (each parent category can contain up to 50 channels), for threads: id of the text channel this thread was created [OPTIONAL & NULLABLE]
    -- channelLastPinTimestamp                        -- when the last pinned message was pinned. This may be null in events such as `GUILD_CREATE` when a message is not pinned [OPTIONAL & NULLABLE]
    -- channelVoiceRegion                             -- voice region id for the voice channel, automatic when set to null [OPTIONAL & NULLABLE]
    -- channelVideoQualityMode                        -- the camera video quality mode of the voice channel, 1 (AUTO) when not present [OPTIONAL]
    -- channelMessageCount         :: Maybe Integer   -- an approximate count of messages in a thread, stops counting at 50 [OPTIONAL]
    , channelMemberCount          :: Maybe Integer    -- an approximate count of users in a thread, stops counting at 50 [OPTIONAL]
    -- channelThreadMetadata                          -- thread-specific fields not needed by other channels [OPTIONAL]
    -- channelMember                                  -- thread member object for the current user, if they have joined the thread, only included on certain API endpoints [OPTIONAL]
    -- channelDefaultAutoArchiveDuration              -- default duration that the clients (not the API) will use for newly created threads, in minutes, to automatically archive the thread after recent activity, can be set to: 60, 1440, 4320, 10080 [OPTIONAL]
    , channelPermissions          :: Maybe Text       -- computed permissions for the invoking user in the channel, including overwrites, only included when part of the `resolved` data received on a slash command interaction [OPTIONAL]
    } deriving (Show)

instance Eq Channel where
    (==) = (==) `on` channelId



newtype GuildUnavailable = 
    GuildUnavailable Snowflake deriving (Show, Eq)