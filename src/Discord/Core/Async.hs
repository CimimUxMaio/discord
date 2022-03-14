{-# LANGUAGE NamedFieldPuns #-}

module Discord.Core.Async where
import Discord.Core.Internal.Types (BotEnv(_envWaitingTasks), BotAction, WaitingTask (..))
import Control.Concurrent.STM (modifyTVar, atomically, newEmptyTMVarIO, takeTMVar, STM, TVar, isEmptyTMVar, putTMVar, TMVar, tryPutTMVar)
import Control.Monad.RWS (asks, MonadIO (liftIO))
import Discord.API.Internal.Types.BotEvent (BotEvent (MessageReactionAdd))
import qualified Discord.API.Internal.Types.BotEvent as API
import Control.Concurrent (myThreadId, ThreadId, threadDelay, forkIO)
import Data.Maybe (fromMaybe, isJust)
import Control.Monad (void, when)
import Discord.API.Internal.Types.Common (Snowflake)
import Discord.API.Internal.Types.Guild (GuildMember (guildMemberUser), Emoji (emojiId, Emoji, emojiName))
import Discord.API.Internal.Types.User (User(User, userId))
import Data.Functor ((<&>))
import Data.Text (Text)


sleep :: Int -> BotAction s ()
sleep = liftIO . threadDelay


waitFor :: (BotEvent -> Maybe a) -> (a -> Bool) -> Int -> BotAction s (Maybe a)
waitFor parser condition timeout = do
    container <- liftIO newEmptyTMVarIO
    tid <- liftIO myThreadId
    let wt = WaitingTask { wtMatches   = matcher
                         , wtContainer = container
                         , wtThreadId  = tid
                         }

    waitingTasks <- asks _envWaitingTasks
    liftIO . atomically $ registerWaitingTask waitingTasks wt

    setTimeout container

    liftIO . atomically $ do
        result <- takeTMVar container
        unRegisterWaitingTask waitingTasks tid
        pure $ result >>= parser

    where
        matcher :: BotEvent -> Bool
        matcher = maybe False condition . parser --fromMaybe False (parser event <&> condition)

        registerWaitingTask :: TVar [WaitingTask] -> WaitingTask -> STM ()
        registerWaitingTask waitingTasks wt = modifyTVar waitingTasks (wt :)

        unRegisterWaitingTask :: TVar [WaitingTask] -> ThreadId -> STM ()
        unRegisterWaitingTask waitingTasks tid = modifyTVar waitingTasks (filter (\wt -> wtThreadId wt /= tid))

        setTimeout :: TMVar (Maybe BotEvent) -> BotAction s ()
        setTimeout container = void . liftIO . forkIO $ do
            threadDelay timeout
            void . atomically $ tryPutTMVar container Nothing




data ReactionInfo = ReactionInfo
    { riMessageId :: Snowflake
    , riUser      :: Maybe User
    , riEmoji     :: Emoji
    }


waitReactionWith :: Int -> Snowflake -> (ReactionInfo -> Bool) -> BotAction s (Maybe ReactionInfo)
waitReactionWith timeout msgId condition =
    waitFor toReactionInfo finalCondition timeout

    where
        finalCondition :: ReactionInfo -> Bool
        finalCondition info = riMessageId info == msgId && condition info

        toReactionInfo :: BotEvent -> Maybe ReactionInfo
        toReactionInfo (MessageReactionAdd _ (API.ReactionInfo msgId _ _) guildMember emoji) =
            let info = ReactionInfo { riMessageId = msgId
                                    , riUser      = guildMember >>= guildMemberUser
                                    , riEmoji     = emoji
                                    }
            in Just info

        toReactionInfo _ = Nothing


waitReaction :: Int -> Snowflake -> BotAction s (Maybe ReactionInfo)
waitReaction timeout msgId = waitReactionWith timeout msgId (const True)


waitReactionEmojiWith :: Int -> Snowflake -> (Emoji -> Bool) -> BotAction s (Maybe ReactionInfo)
waitReactionEmojiWith timeout msgId condition =
    waitReactionWith timeout msgId (condition . riEmoji)

waitReactionEmojiWithId :: Int -> Snowflake -> Snowflake -> BotAction s (Maybe ReactionInfo)
waitReactionEmojiWithId timeout msgId emojiId' = waitReactionEmojiWith timeout msgId $ \info ->
    Just emojiId' == emojiId info

waitReactionEmojiWithName :: Int -> Snowflake -> Text -> BotAction s (Maybe ReactionInfo)
waitReactionEmojiWithName timeout msgId emojiName' = waitReactionEmojiWith timeout msgId $ \info ->
    Just emojiName' == emojiName info


waitReactionUserWith :: Int -> Snowflake -> (User -> Bool) -> BotAction s (Maybe ReactionInfo)
waitReactionUserWith timeout msgId condition =
    waitReactionWith timeout msgId (maybe False condition . riUser)


waitReactionUserWithId :: Int -> Snowflake -> Snowflake -> BotAction s (Maybe ReactionInfo)
waitReactionUserWithId timeout msgId userId' = 
    waitReactionUserWith timeout msgId $ \user ->
        userId user == userId'