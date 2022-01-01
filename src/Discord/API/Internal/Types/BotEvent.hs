module Discord.API.Internal.Types.BotEvent where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Discord.API.Internal.Types.Channel (Channel)
import Discord.API.Internal.Types.User (User)
import Discord.API.Internal.Types.Guild (GuildUnavailable, Guild, Emoji, Sticker, GuildMember)
import Discord.API.Internal.Types.Common (Snowflake)
import Data.Time (UTCTime)


type SessionID = Text

newtype GatewayVersion = GatewayVersion Integer deriving (Show, Eq, Generic, FromJSON)

data BotEvent = Ready GatewayVersion User [GuildUnavailable] SessionID
              | Resumed [Text]
              | ChannelCreate Channel
              | ChannelUpdate Channel
              | ChannelDelete Channel
              | ChannelPinsUpdate Snowflake (Maybe UTCTime)
              | GuildCreate Guild
              | GuildUpdate Guild
              | GuildBanAdd Guild
              | GuildBanRemove Guild
              | GuildEmojisUpdate Snowflake [Emoji]
              | GuildStickersUpdate Snowflake [Sticker]
              | GuildIntegrationsUpdate Snowflake
              | GuildMemberAdd Snowflake GuildMember
              | GuildMemberUpdate Snowflake GuildMember 
              deriving (Show)
