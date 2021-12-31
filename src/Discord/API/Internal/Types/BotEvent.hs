module Discord.API.Internal.Types.BotEvent where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Discord.API.Internal.Types.Channel (Channel)
import Discord.API.Internal.Types.User (User)
import Discord.API.Internal.Types.Guild (GuildUnavailable)


type SessionID = Text

newtype GatewayVersion = GatewayVersion Integer deriving (Show, Eq, Generic, FromJSON)

data BotEvent = Ready GatewayVersion User [GuildUnavailable] SessionID
              | ChannelCreate Channel
              | ChannelUpdate Channel
              | ChannelDelete Channel
              | ChannelPinsUpdate Channel
              deriving (Show)
