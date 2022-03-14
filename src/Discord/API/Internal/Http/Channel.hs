module Discord.API.Internal.Http.Channel where
import Discord.API.Internal.Types.Common (Snowflake (Snowflake))
import Discord.API.Internal.Types.Channel (Channel)
import Discord.API.Internal.Http.Common (getApi, patchApi, deleteApi, (=:?), postApi, AuthorizedRequest, putApi, putApi_, deleteApi_)
import Discord.API.Internal.Types.Message ( Message )
import Network.HTTP.Req ((=:), ReqBodyJson (ReqBodyJson), NoReqBody (NoReqBody))
import Discord.API.Internal.Http.Types (SendableMessage)
import Data.Text (Text)
import Discord.API.Internal.Types.Guild (Emoji (emojiName))
import Discord.API.Internal.Types.User (User)
import Discord.API.Internal.ToMsgFormat (ToMsgFormat(toMsgFormat))



{- GETs -}

getChannel :: Snowflake -> AuthorizedRequest Channel
getChannel (Snowflake chid) = getApi ["channels", chid] mempty


getMessagesDef :: Snowflake -> AuthorizedRequest [Message]
getMessagesDef chid = getMessages chid Nothing Nothing Nothing Nothing

getMessages :: Snowflake -> Maybe Snowflake -> Maybe Snowflake -> Maybe Snowflake -> Maybe Integer -> AuthorizedRequest [Message]
getMessages (Snowflake chid) around before after limit =
    getApi ["channels", chid, "messages"] opts

    where opts = "around" =:? around
              <> "before" =:? before
              <> "after"  =:? after
              <> "limit"  =:? limit


getMessage :: Snowflake -> Snowflake -> AuthorizedRequest Message
getMessage (Snowflake chid) (Snowflake mid) =
    getApi ["channels", chid, "messages", mid] mempty


getReactions :: Snowflake -> Snowflake -> Emoji -> Maybe Snowflake -> Maybe Int -> AuthorizedRequest [User]
getReactions (Snowflake chid) (Snowflake mid) emoji after limit =
    getApi ["channels", chid, "messages", mid, "reactions", toMsgFormat emoji] options

    where
        options = "after" =:? after
               <> "limit" =:? limit


{- DELETEs -}

deleteChannel :: Snowflake -> AuthorizedRequest Channel
deleteChannel (Snowflake chid) = deleteApi  ["channels", chid]


deleteOwnReaction :: Snowflake -> Snowflake -> Emoji -> AuthorizedRequest ()
deleteOwnReaction (Snowflake chid) (Snowflake mid) emoji =
    deleteApi_ ["channels", chid, "messages", mid, "reactions", toMsgFormat emoji, "@me"]


deleteUserReaction :: Snowflake -> Snowflake -> Emoji -> Snowflake -> AuthorizedRequest ()
deleteUserReaction (Snowflake chid) (Snowflake mid) emoji (Snowflake uid) =
    deleteApi_ ["channels", chid, "messages", mid, "reactions", toMsgFormat emoji, uid]


deleteAllReactions :: Snowflake -> Snowflake -> AuthorizedRequest ()
deleteAllReactions (Snowflake chid) (Snowflake mid) =
    deleteApi_ ["channels", chid, "messages", mid, "reactions"]


deleteAllReactionsForEmoji :: Snowflake -> Snowflake -> Emoji -> AuthorizedRequest ()
deleteAllReactionsForEmoji (Snowflake chid) (Snowflake mid) emoji =
    deleteApi_ ["channels", chid, "messages", mid, "reactions", toMsgFormat emoji]


{- POSTs -}

sendMessage :: Snowflake -> SendableMessage -> AuthorizedRequest Message
sendMessage (Snowflake chid) =
    postApi ["channels", chid, "messages"] . ReqBodyJson



{- PUTs -}

createReaction :: Snowflake -> Snowflake -> Emoji -> AuthorizedRequest ()
createReaction (Snowflake chid) (Snowflake mid) emoji =
    putApi_ ["channels", chid, "messages", mid, "reactions", toMsgFormat emoji, "@me"] NoReqBody