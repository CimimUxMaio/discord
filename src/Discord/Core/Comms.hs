{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Discord.Core.Comms 
( send
, sendText
, sendEmbeds
, sendStickers
, addReaction ) where

import Discord.Core.Internal.Types (BotConfig (token), BotAction, BotEnv (envConfig))
import Data.Text (Text)
import Control.Monad.RWS (asks, MonadIO (liftIO))
import Discord.API.Internal.Http.Channel (sendMessage, createReaction, deleteOwnReaction, deleteUserReaction, deleteAllReactions, deleteAllReactionsForEmoji)
import Discord.API.Internal.Types.Message (Message (messageChannelId, Message, messageId))
import Discord.API.Internal.Http.Types (SendableMessage (SendableText, SendableEmbeds, SendableStickers))
import Discord.API.Internal.Types.Common (Snowflake)
import Discord.API.Internal.Types.Embed (Embed)
import Discord.API.Internal.Types.Guild (Emoji)
import Discord.API.Internal.Types.User (User (..))


liftWithToken :: (Text -> IO a) -> BotAction s a
liftWithToken f = asks (token . envConfig) >>= (liftIO . f)


-- | Sends a sendable value to the given channel id.
send :: Snowflake            -- ^ Text channel ID
     -> SendableMessage      -- ^ Sendable
     -> BotAction s Message  -- ^ Action
send chid = liftWithToken . sendMessage chid


{- | 'send' wrapper.
Sends text to the given channel id.
-}
sendText :: Snowflake            -- ^ Text channel ID
         -> Text                 -- ^ Text to be sent
         -> BotAction s Message  -- ^ Action
sendText chid = send chid . SendableText


{- | 'send' wrapper.
Sends embeds to the given channel id.
-}
sendEmbeds :: Snowflake            -- ^ Text channel ID
           -> [Embed]              -- ^ Embed list
           -> BotAction s Message  -- ^ Action
sendEmbeds chid = send chid . SendableEmbeds


{- | 'send' wrapper.
Sends stickers to the given channel id.
-}
sendStickers :: Snowflake            -- ^ Text channel ID
             -> [Snowflake]          -- ^ Sticker list
             -> BotAction s Message  -- ^ Action
sendStickers chid = send chid . SendableStickers


-- | Adds a reaction to the given message with the given emoji.
addReaction :: Message         -- ^ Message to react to 
            -> Emoji           -- ^ Emoji to react with
            -> BotAction s ()  -- ^ Action
addReaction Message{..} = 
    liftWithToken . createReaction messageChannelId messageId


removeOwnReaction :: Message 
                  -> Emoji 
                  -> BotAction s ()
removeOwnReaction Message{..} = 
    liftWithToken . deleteOwnReaction messageChannelId messageId


removeUserReaction :: Message 
                   -> Emoji 
                   -> User 
                   -> BotAction s ()
removeUserReaction Message{..} emoji User{userId} =
    liftWithToken $ deleteUserReaction messageChannelId messageId emoji userId


removeAllReactions :: Message 
                   -> BotAction s ()
removeAllReactions Message{..} =
    liftWithToken $ deleteAllReactions messageChannelId messageId


removeAllReactionsForEmoji :: Message 
                           -> Emoji 
                           -> BotAction s ()
removeAllReactionsForEmoji Message{..} =
    liftWithToken . deleteAllReactionsForEmoji messageChannelId messageId
