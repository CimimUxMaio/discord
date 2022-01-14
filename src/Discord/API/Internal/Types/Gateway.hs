{-# LANGUAGE RecordWildCards #-}
module Discord.API.Internal.Types.Gateway where

import Data.Text (Text)
import Data.Default (Default (def))
import Discord.API.Internal.Types.BotEvent (BotEvent)
import Data.Aeson (withObject, FromJSON (parseJSON), (.:), Value (Object, Number), ToJSON (toJSON), object, (.=), (.:?))
import Data.Aeson.Types (Parser)
import System.Info (os)
import Discord.API.Internal.Types.Common (Snowflake)
import Data.Scientific (scientific)



data GatewayIntent = GatewayIntent
  { gatewayIntentGuilds                 :: Bool
  , gatewayIntentMembers                :: Bool
  , gatewayIntentBans                   :: Bool
  , gatewayIntentEmojis                 :: Bool
  , gatewayIntentIntegrations           :: Bool
  , gatewayIntentWebhooks               :: Bool
  , gatewayIntentInvites                :: Bool
  , gatewayIntentVoiceStates            :: Bool
  , gatewayIntentPrecenses              :: Bool
  , gatewayIntentMessageChanges         :: Bool
  , gatewayIntentMessageReactions       :: Bool
  , gatewayIntentMessageTyping          :: Bool
  , gatewayIntentDirectMessageChanges   :: Bool
  , gatewayIntentDirectMessageReactions :: Bool
  , gatewayIntentDirectMessageTyping    :: Bool
  } deriving (Show, Eq)

instance Default GatewayIntent where
  def = GatewayIntent { gatewayIntentGuilds                 = True
                      , gatewayIntentMembers                = False
                      , gatewayIntentBans                   = True
                      , gatewayIntentEmojis                 = True
                      , gatewayIntentIntegrations           = True
                      , gatewayIntentWebhooks               = True
                      , gatewayIntentInvites                = True
                      , gatewayIntentVoiceStates            = True
                      , gatewayIntentPrecenses              = False
                      , gatewayIntentMessageChanges         = True
                      , gatewayIntentMessageReactions       = True
                      , gatewayIntentMessageTyping          = True
                      , gatewayIntentDirectMessageChanges   = True
                      , gatewayIntentDirectMessageReactions = True
                      , gatewayIntentDirectMessageTyping    = True
                      }


compileGatewayIntent :: GatewayIntent -> Integer
compileGatewayIntent GatewayIntent{..} =
 sum $ [ if on then flag else 0
       | (flag, on) <- [ (1, gatewayIntentGuilds)
                       , (2 ^  1, gatewayIntentMembers)
                       , (2 ^  2, gatewayIntentBans)
                       , (2 ^  3, gatewayIntentEmojis)
                       , (2 ^  4, gatewayIntentIntegrations)
                       , (2 ^  5, gatewayIntentWebhooks)
                       , (2 ^  6, gatewayIntentInvites)
                       , (2 ^  7, gatewayIntentVoiceStates)
                       , (2 ^  8, gatewayIntentPrecenses)
                       , (2 ^  9, gatewayIntentMessageChanges)
                       , (2 ^ 10, gatewayIntentMessageReactions)
                       , (2 ^ 11, gatewayIntentMessageTyping)
                       , (2 ^ 12, gatewayIntentDirectMessageChanges)
                       , (2 ^ 13, gatewayIntentDirectMessageReactions)
                       , (2 ^ 14, gatewayIntentDirectMessageTyping)
                       ]
       ]

gatewayPayload :: ToJSON a => Integer -> a -> Value
gatewayPayload opcode data' = object [ "op" .= opcode
                                     , "d"  .= data'
                                     ]


data GatewayReceivable = Dispatch         BotEvent (Maybe Integer)
                       | HeartbeatRequest
                       | Reconnect
                       | InvalidSession
                       | Hello            Int
                       | HeartbeatACK
                       deriving (Show)

instance FromJSON GatewayReceivable where
    parseJSON = withObject "GatewayReceivable" $ \o -> do
        let json = Object o
        op <- o .: "op" :: Parser Integer
        case op of
            0  -> Dispatch <$> parseJSON json
                           <*> o .:? "s"
            1  -> pure HeartbeatRequest 
            7  -> pure Reconnect
            9  -> pure InvalidSession
            10 -> do
                payload <- o .: "d"
                Hello <$> payload .: "heartbeat_interval"
            11 -> pure HeartbeatACK
            _  -> fail "unknown gateway receivable"


data GatewaySendableInternal = Heartbeat (Maybe Integer)    -- Last sequence number
                             | Identify  Text Integer       -- Token, Intents
                             | Resume    Text Text Integer  -- Token, Session ID, Last sequence number
                             deriving (Show, Eq)

instance ToJSON GatewaySendableInternal where
    toJSON (Heartbeat lastSeq) = gatewayPayload 1 lastSeq
    
    toJSON (Identify token intents) = 
        gatewayPayload 2 $ object [ "token"      .= token
                                  , "properties" .= object [ "$os"      .= os
                                                           , "$browser" .= ("discord-hs" :: Text)
                                                           , "$device"  .= ("discord-hs" :: Text) 
                                                           ] 
                                  , "intents"    .= intents
                                  ]
    
    toJSON (Resume token sessionId lastSeq) =
        gatewayPayload 6 $ object [ "token" .= token
                                  , "session_id" .= sessionId
                                  , "seq" .= lastSeq 
                                  ]


data ActivityType = Game
                  | Streaming
                  | Listening
                  | Watching
                  | Custom
                  | Competing
                  deriving (Show, Enum)

instance ToJSON ActivityType where
    toJSON = Number . (`scientific` 0) . toInteger . fromEnum


data Activity = Activity
    { activityName :: Text
    , activityType :: ActivityType
    , activityUrl  :: Maybe Text 
    } deriving (Show)

instance ToJSON Activity where
    toJSON Activity{..} = object [ "name" .= activityName
                                 , "type" .= activityType
                                 , "url"  .= activityUrl 
                                 ]


data GatewaySendable = RequestGuildMembers Snowflake Text Integer                 -- Guild_id, Query, Limit
                     | VoiceStateUpdate    Snowflake (Maybe Snowflake) Bool Bool  -- Guild id, channel_id, muted, deafened
                     | PresenceUpdate      (Maybe Integer) [Activity] Text Bool   -- Since, Activities, Status, AFK
                     deriving (Show)


instance ToJSON GatewaySendable where
    toJSON (RequestGuildMembers guildId' query limit) = 
        gatewayPayload 8 $ object [ "guild_id" .= guildId'
                                  , "query"    .= query
                                  , "limit"    .= limit
                                  ]
    
    toJSON (VoiceStateUpdate guildId' channelId' muted deafened) = 
        gatewayPayload 4 $ object [ "guild_id"   .= guildId'
                                  , "channel_id" .= channelId'
                                  , "self_mute"  .= muted
                                  , "self_deaf"  .= deafened
                                  ]
    
    toJSON (PresenceUpdate since activities status afk) =
        gatewayPayload 3 $ object [ "since"      .= since
                                  , "activities" .= activities
                                  , "status"     .= status
                                  , "afk"        .= afk 
                                  ]