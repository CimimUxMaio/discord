module Main where

import Discord.Core.Internal.Types (BotConfig (..), BotApp (..), BotM)
import Discord.Core.Bot (startBot)
import Discord.Core.Handlers (onCommand)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Discord.API.Internal.Types.Message (Message(messageChannelId, messageAuthor))
import Discord.API.Internal.Types.Embed (Embed, EmbedField (EmbedField))
import Discord.API.Internal.Http.Channel (sendMessage)
import Control.Monad.Reader (asks)
import Discord.Core.Comms (sendText, sendEmbeds)
import Discord.Core.Embeds.Builder (runEmbedBuilder, description, title, thumbnail, author, authorIconUrl, authorUrl, footer, footerIconUrl, image, color, fields)
import Discord.API.Internal.Types.User (User(userId, userName, userAvatarUrl))
import Discord.Core.Embeds.Colors (cyan, cornflowerblue, tomato)


customConfig :: BotConfig
customConfig = BotConfig
    { prefix = "?"
    , token = "<your_token>" 
    }

app :: BotApp
app = BotApp
    { config  = customConfig 
    , handler = customHandler 
    }
    

customEmbed :: Maybe User -> Embed
customEmbed blame = runEmbedBuilder $ do 
    title "A nice title"
    description "a nicer embed description"
    color tomato

    thumbnail "https://cryptocurrency-nieuws.nl/wp-content/uploads/2022/01/kan-cardano-de-2-halen-deze-week.jpg"
    image "https://cryptocurrency-nieuws.nl/wp-content/uploads/2022/01/kan-cardano-de-2-halen-deze-week.jpg"

    fields [ EmbedField "field title 1" "value 1" False
           , EmbedField "inline field title 2" "value 2" True 
           , EmbedField "inline field title 3" "value 3" True 
           ]

    case blame of
        Nothing -> author "anonymous" $ pure ()
        Just u  -> do
            author (userName u) $ do
                authorIconUrl (userAvatarUrl u)
            
            footer "Some footer text" $
                footerIconUrl (userAvatarUrl u)


customHandler :: BotM ()
customHandler = do
    onCommand "embed" $ \(msg, args) -> do
        let chid = messageChannelId msg
        let blame = messageAuthor msg
        sendEmbeds chid [ customEmbed blame ]
        liftIO $ print "Embed sent!"

    onCommand "ping" $ \(msg, args) -> do
        let chid = messageChannelId msg
        sendText chid "Pong!"
        liftIO $ print "Pong!"


main :: IO ()
main = do
    startBot app