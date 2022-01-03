module Discord.API.Internal.Types.BotEvent where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, (.:?), Object, Value (Object))
import Discord.API.Internal.Types.Channel (Channel)
import Discord.API.Internal.Types.User (User)
import Discord.API.Internal.Types.Guild (GuildUnavailable, Guild, Emoji, Sticker, GuildMember, Role)
import Discord.API.Internal.Types.Common (Snowflake)
import Data.Time (UTCTime)
import Discord.API.Internal.Types.Message (Message)
import Data.Aeson.Types ( (.:), withObject, FromJSON(parseJSON), Parser )


type SessionID = Text

newtype GatewayVersion = GatewayVersion Integer deriving (Show, Eq, Generic, FromJSON)

data BotEvent = Ready                      GatewayVersion User [GuildUnavailable] SessionID
              | Resumed                    
              | ChannelCreate              Channel
              | ChannelUpdate              Channel
              | ChannelDelete              Channel
              | ChannelPinsUpdate          (Maybe Snowflake) Snowflake (Maybe UTCTime)
              | GuildCreate                Guild
              | GuildUpdate                Guild
              | GuildDelete                GuildUnavailable
              | GuildBanAdd                Snowflake User
              | GuildBanRemove             Snowflake User
              | GuildEmojisUpdate          Snowflake [Emoji]
              | GuildStickersUpdate        Snowflake [Sticker]
              | GuildIntegrationsUpdate    Snowflake
              | GuildMemberAdd             Snowflake GuildMember
              | GuildMemberRemove          Snowflake User
              | GuildMemberUpdate          Snowflake [Snowflake] User (Maybe Text)  -- guild_id, roles, user, nick 
              | GuildMemberChunk           Snowflake [GuildMember]
              | GuildRoleCreate            Snowflake Role
              | GuildRoleUpdate            Snowflake Role
              | GuildRoleDelete            Snowflake Snowflake                      -- guild_id, role_id
              | MessageCreate              Message
              | MessageUpdate              Message
              | MessageDelete              Snowflake Snowflake (Maybe Snowflake)    -- message id, channel_id, guild_id?
              | MessageDeleteBulk          [Snowflake] Snowflake (Maybe Snowflake)  -- message ids, channel_id, guild_id?
              | MessageReactionAdd         Snowflake ReactionInfo (Maybe GuildMember) Emoji
              | MessageReactionRemove      Snowflake ReactionInfo Emoji
              | MessageReactionRemoveAll   ReactionInfo                             -- channel_id, message_id, guild_id
              | MessageReactionRemoveEmoji ReactionInfo Emoji
              | PresenceUpdate             PresenceInfo
              | TypingStart                TypingInfo
              | UserUpdate                 User
              | UnhandledEvent             Text
              deriving (Show)

instance FromJSON BotEvent where
    parseJSON = withObject "BotEvent" $ \o -> do
        type_ <- o .: "t" :: Parser Text
        payload <- o .: "d" :: Parser Object
        let payloadObject = Object payload

        case type_ of
            "READY" -> Ready <$> payload .: "v"
                             <*> payload .: "user"
                             <*> payload .: "guilds"
                             <*> payload .: "session_id"

            "RESUMED" -> pure Resumed
            "CHANNEL_CREATE" -> ChannelCreate <$> parseJSON payloadObject
            "CHANNEL_UPDATE" -> ChannelUpdate <$> parseJSON payloadObject
            "CHANNEL_DELETE" -> ChannelDelete <$> parseJSON payloadObject

            "CHANNEL_PINS_UPDATE" -> ChannelPinsUpdate <$> payload .:? "guild_id"
                                                       <*> payload .:  "channel_id"
                                                       <*> payload .:? "last_pin_timestamp"

            "GUILD_CREATE" -> GuildCreate <$> parseJSON payloadObject
            "GUILD_UPDATE" -> GuildUpdate <$> parseJSON payloadObject
            "GUILD_DELETE" -> GuildDelete <$> parseJSON payloadObject

            "GUILD_BAN_ADD" -> GuildBanAdd <$> payload .: "guild_id"
                                           <*> payload .: "user"

            "GUILD_BAN_REMOVE" -> GuildBanRemove <$> payload .: "guild_id"
                                                 <*> payload .: "user"

            "GUILD_EMOJI_UPDATE" -> GuildEmojisUpdate <$> payload .: "guild_id"
                                                      <*> payload .: "emojis"

            "GUILD_INTEGRATIONS_UPDATE" -> GuildIntegrationsUpdate <$> payload .: "guild_id"

            "GUILD_MEMBER_ADD" -> GuildMemberAdd <$> payload .: "guild_id"
                                                 <*> parseJSON payloadObject

            "GUILD_MEMBER_REMOVE" -> GuildMemberRemove <$> payload .: "guild_id"
                                                       <*> payload .: "user"

            "GUILD_MEMBER_UPDATE" -> GuildMemberUpdate <$> payload .:  "guild_id"
                                                       <*> payload .:  "roles"
                                                       <*> payload .:  "user"
                                                       <*> payload .:? "nick"

            "GUILD_MEMBERS_CHUNK" -> GuildMemberChunk <$> payload .: "guild_id"
                                                      <*> payload .: "members"

            "GUILD_ROLE_CREATE" -> GuildRoleCreate <$> payload .: "guild_id"
                                                   <*> payload .: "role"

            "GUILD_ROLE_UPDATE" -> GuildRoleUpdate <$> payload .: "guild_id"
                                                   <*> payload .: "role"

            "GUILD_ROLE_DELETE" -> GuildRoleDelete <$> payload .: "guild_id"
                                                   <*> payload .: "role_id"

            "MESSAGE_CREATE" -> MessageCreate <$> parseJSON payloadObject
            "MESSAGE_UPDATE" -> MessageUpdate <$> parseJSON payloadObject

            "MESSAGE_DELETE" -> MessageDelete <$> payload .:  "id"
                                              <*> payload .:  "channel_id"
                                              <*> payload .:? "guild_id"

            "MESSAGE_DELETE_BULK" -> MessageDeleteBulk <$> payload .:  "ids"
                                                       <*> payload .:  "channel_id"
                                                       <*> payload .:? "guild_id"

            "MESSAGE_REACTION_ADD" -> MessageReactionAdd <$> payload .: "user_id"
                                                         <*> parseJSON payloadObject
                                                         <*> payload .:? "member"
                                                         <*> payload .: "emoji"

            "MESSAGE_REACTION_REMOVE" -> MessageReactionRemove <$> payload .: "user_id"
                                                               <*> parseJSON payloadObject
                                                               <*> payload .: "emoji"

            "MESSAGE_REACTION_REMOVE_ALL" -> MessageReactionRemoveAll <$> parseJSON payloadObject

            "MESSAGE_REACTION_REMOVE_EMOJI" -> MessageReactionRemoveEmoji <$> parseJSON payloadObject
                                                                          <*> payload .: "emoji"

            "PRESENCE_UPDATE" -> PresenceUpdate <$> parseJSON payloadObject

            "TYPING_START" -> TypingStart <$> parseJSON payloadObject

            "USER_UPDATE" -> UserUpdate <$> parseJSON payloadObject

            other -> pure $ UnhandledEvent other


data ReactionInfo = ReactionInfo
    { reactionMessageId :: Snowflake
    , reactionChannelId :: Snowflake
    , reactionGuildId   :: Maybe Snowflake
    } deriving Show

instance FromJSON ReactionInfo where
    parseJSON = withObject "ReactionInfo" $ \o ->
        ReactionInfo <$> o .:  "message_id"
                     <*> o .:  "channel_id"
                     <*> o .:? "guild_id"


data PresenceInfo = PresenceInfo
    { presenceUser    :: User
    , presenceGuildId :: Snowflake
    , presenceStatus  :: Text
    } deriving Show

instance FromJSON PresenceInfo where
    parseJSON = withObject "PresenceInfo" $ \o ->
        PresenceInfo <$> o .: "user"
                     <*> o .: "guild_id"
                     <*> o .: "status"


data TypingInfo = TypingInfo
    { typingUserId :: Snowflake
    , typingChannelId :: Snowflake
    , typingTimestamp :: UTCTime
    } deriving Show

instance FromJSON TypingInfo where
    parseJSON = withObject "TypingInfo" $ \o ->
        TypingInfo <$> o .: "user_id"
                   <*> o .: "channel_id"
                   <*> o .: "timestamp"