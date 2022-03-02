module Utils where

import Discord.API.Internal.Types.Message
    ( Message(..) )
import Discord.API.Internal.Types.Common (Snowflake(Snowflake))
import Data.Text (Text)


simpleMessage :: Text -> Message
simpleMessage content = Message
    { messageId               = Snowflake "asdasdasdasdasd"      
    , messageChannelId        = Snowflake "aasdjkasjdkajsa"      
    , messageAuthor           = Nothing       
    , messageText             = content
    , messageTimestamp        = undefined          
    , messageLastEdition      = Nothing    
    , messageMentionsEveryone = False      
    , messageMentions         = []         
    , messageMentionedRoles   = []
    , messageAttachments      = []
    , messageEmbeds           = []
    , messageReactions        = []
    , messageIsPinned         = False             
    , messageGuildId          = Nothing   
    , referencedMessage       = Nothing   
    }