module Discord.API.Internal.Types.Message where
import Discord.API.Internal.Types.Common (Snowflake)
import Discord.API.Internal.Types.User (User)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Aeson (FromJSON, withObject, (.:), (.:?), (.!=))
import Data.Aeson.Types (FromJSON(parseJSON))
import Data.Function (on)
import Discord.API.Internal.Types.Guild (Emoji)
import Discord.API.Internal.Types.Embed (Embed)


data Message = Message
    { messageId               :: Snowflake        -- id of the message
    , messageChannelId        :: Snowflake        -- id of the channel the message was sent in
    , messageAuthor           :: Maybe User       -- the author of this message
    , messageText             :: Text             -- contents of the message
    , messageTimestamp        :: UTCTime          -- when this message was sent
    , messageLastEdition      :: Maybe UTCTime    -- when this message was edited
    , messageMentionsEveryone :: Bool             -- whether this message mentions everyone
    , messageMentions         :: [User]           -- users specifically mentioned in the message
    , messageMentionedRoles   :: [Snowflake]      -- roles specifically mentioned in the message
    , messageAttachments      :: [Attachment]     -- any attached files
    , messageEmbeds           :: [Embed]          -- any embedded content
    , messageReactions        :: [Reaction]       -- reactions to the message
    , messageIsPinned         :: Bool             -- whether this message is pinned
    , messageGuildId          :: Maybe Snowflake  -- id of the guild the message was sent in
    , referencedMessage       :: Maybe Message    -- referenced message (cross-post, channel follow add, pin, or reply message)
    } deriving (Show)

instance Eq Message where
    (==) = (==) `on` messageId

instance FromJSON Message where
    parseJSON = withObject "Message" $ \o ->
        Message <$> o .:  "id"
                <*> o .:  "channel_id"
                <*> (do isWebhookMsg <- o .:? "webhook_id"
                        author <- o .: "author"
                        case isWebhookMsg :: Maybe Snowflake of
                            Nothing -> pure author
                            Just _  -> pure Nothing )
                <*> o .:? "content" .!= ""
                <*> o .:  "timestamp"
                <*> o .:? "edited_timestamp"
                <*> o .:? "mention_everyone" .!= False
                <*> o .:? "mentions" .!= []
                <*> o .:? "mention_roles" .!= []
                <*> o .:? "attachments" .!= []
                <*> o .:  "embeds"
                <*> o .:? "reactions" .!= []
                <*> o .:? "pinned" .!= False 
                <*> o .:? "guild_id"
                <*> o .:? "referenced_message"


data Attachment = Attachment 
    { attachmentId       :: Snowflake      -- attachment id
    , attachmentFilename :: Text           -- name of file attached
    , attachmentSize     :: Integer        -- size of file in bytes
    , attachmentUrl      :: Text           -- source url of file
    , attachmentProxyUrl :: Text           -- a proxied url of file
    , attachmentHeight   :: Maybe Integer  -- height of file (if image)
    , attachmentWidth    :: Maybe Integer  -- width of file (if image)
    , attachmentType     :: Maybe Text     -- the attachment's media type
    } deriving Show

instance FromJSON Attachment where
    parseJSON = withObject "Attachment" $ \o ->
        Attachment <$> o .:  "id"
                   <*> o .:  "filename"
                   <*> o .:  "size"
                   <*> o .:  "url"
                   <*> o .:  "proxy_url"
                   <*> o .:? "height"
                   <*> o .:? "width"
                   <*> o .:? "content_type"


data Reaction = Reaction 
    { reactionCount        :: Integer  -- times this emoji has been used to react
    , reactionIsMeIncluded :: Bool     -- whether the current user reacted using this emoji
    , reactionEmoji        :: Emoji    -- emoji information
    } deriving Show

instance FromJSON Reaction where
    parseJSON = withObject "Reaction" $ \o ->
        Reaction <$> o .: "count"
                 <*> o .: "me"
                 <*> o .: "emoji"