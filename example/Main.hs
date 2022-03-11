{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Main where

import Discord.Core.Internal.Types
    ( BotConfig(..),
      BotApp(..),
      BotM,
      BotApp(appInitialState, appExceptionHandlers),
      BotExceptionHandler(BotExceptionHandler) )
import Discord.Core.Bot (startBot)
import Discord.Core.Handlers (onCommand)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Discord.API.Internal.Types.Message (Message(..))
import Discord.API.Internal.Types.Embed (Embed)
import Discord.API.Internal.Types.Guild (Emoji (..))
import Discord.API.Internal.Http.Channel (sendMessage)
import Control.Monad.Reader (asks)
import Discord.Core.Comms (sendText, sendEmbeds)
import Discord.Core.Embeds.Builder (runEmbedBuilder, description, title, thumbnail, author, authorIconUrl, authorUrl, footer, footerIconUrl, image, color, field)
import Discord.API.Internal.Types.User (User(userId, userName, userAvatarUrl))
import Discord.Core.Embeds.Colors (cyan, cornflowerblue, tomato)
import Control.Monad.RWS (modify, get)
import Control.Exception (throw, ArithException (DivideByZero))
import Control.Monad (void)
import Discord.Core.Context (Context(CommandCtx))
import Data.Text (pack)
import System.Environment (getEnv)
import Data.Maybe (isJust)
import Discord.Core.Async (waitReaction, ReactionInfo (..))


newtype CustomAppState = CustomAppState Int deriving (Show, Num)


app :: BotConfig -> BotApp CustomAppState
app cfg = BotApp
    { appConfig            = cfg
    , appInitialState      = CustomAppState 0
    , appDefinition        = customHandler
    , appExceptionHandlers = exceptionHandlers
    }


exceptionHandlers :: [BotExceptionHandler s]
exceptionHandlers =
    [ BotExceptionHandler $ \ctx (e :: ArithException) -> do
        liftIO $ print "Recovering..."
        case ctx of
            CommandCtx name args msg -> do
                let chid = messageChannelId msg
                void . sendText chid $ "Something went wrong!"
            _ -> pure ()
    ]


customEmbed :: Maybe User -> Embed
customEmbed blame = runEmbedBuilder $ do
    title "A nice title"
    description "a nicer embed description"
    color tomato

    thumbnail "https://cryptocurrency-nieuws.nl/wp-content/uploads/2022/01/kan-cardano-de-2-halen-deze-week.jpg"
    image "https://cryptocurrency-nieuws.nl/wp-content/uploads/2022/01/kan-cardano-de-2-halen-deze-week.jpg"

    field "field title 1" "value 1" False
    field "inline field title 2" "value 2" True
    field "inline field title 3" "value 3" True

    case blame of
        Nothing -> author "anonymous" $ pure ()
        Just u  -> do
            author (userName u) $ do
                authorIconUrl (userAvatarUrl u)

            footer "Some footer text" $
                footerIconUrl (userAvatarUrl u)


customHandler :: BotM CustomAppState ()
customHandler = do
    onCommand "embed" $ \msg args -> do
        let chid = messageChannelId msg
        let blame = messageAuthor msg
        sendEmbeds chid [ customEmbed blame ]
        liftIO $ print "Embed sent!"

    onCommand "ping" $ \msg args -> do
        let chid = messageChannelId msg
        void $ sendText chid "Pong!"

    onCommand "fail" $ \_ _ -> do
        throw DivideByZero

    onCommand "async" $ \msg _ -> do
        let chid = messageChannelId msg
        Message{messageId} <- sendText chid "Hello, please react to this message :D"
        r <- waitReaction 10_000_000 messageId
        let response = case r >>= emojiName . riEmoji of
                Just name -> "Reaction received! Name: " <> name
                Nothing   -> "You didn't react :C"

        void $ sendText chid response


    --onCommand "count" $ \_ _ -> do
    --    modify (+1)
    --    s <- get
    --    liftIO $ print s


main :: IO ()
main = do
    botToken <- getEnv "DISCORD_TOKEN"
    let cfg = BotConfig { prefix = "?", token  = pack botToken }
    startBot (app cfg)