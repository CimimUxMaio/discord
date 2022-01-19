module Discord.API.Internal.Http.Channel where
import Discord.API.Internal.Types.Common (Snowflake (Snowflake))
import Discord.API.Internal.Types.Channel (Channel)
import Discord.API.Internal.Http.Common (getApi, patchApi, deleteApi, (/+), (=:?), postApi, AuthorizedRequest)
import Discord.API.Internal.Types.Message ( Message )
import Network.HTTP.Req ((=:))
import Discord.API.Internal.Http.Types (SendableMessage)
import Data.Text (Text)


{- GETs -}

getChannel :: Snowflake -> AuthorizedRequest Channel
getChannel (Snowflake chid) = getApi ("channels" /+ chid) mempty 


getMessagesDef :: Snowflake -> AuthorizedRequest [Message]
getMessagesDef chid = getMessages chid Nothing Nothing Nothing Nothing

getMessages :: Snowflake -> Maybe Snowflake -> Maybe Snowflake -> Maybe Snowflake -> Maybe Integer -> AuthorizedRequest [Message]
getMessages (Snowflake chid) around before after limit = 
    getApi ("channels" /+ chid /+ "messages") opts 

    where opts = "around" =:? around
              <> "before" =:? before
              <> "after"  =:? after
              <> "limit"  =:? limit


getMessage :: Snowflake -> Snowflake -> AuthorizedRequest [Message]
getMessage (Snowflake chid) (Snowflake mid) =
    getApi ("channels" /+ chid /+ "messages" /+ mid) mempty


{- DELETEs -}

deleteChannel :: Snowflake -> AuthorizedRequest Channel
deleteChannel (Snowflake chid) token = deleteApi token ("channels" /+ chid)


{- POSTs -}

sendMessage :: Snowflake -> SendableMessage -> AuthorizedRequest Message
sendMessage (Snowflake chid) = 
    postApi ("channels" /+ chid /+ "messages")