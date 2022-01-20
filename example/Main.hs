module Main where

import Discord.Core.Internal.Types (BotConfig (..), BotApp (..), BotM)
import Discord.Core.Bot (startBot)
import Discord.Core.Handlers (onCommand)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Discord.API.Internal.Types.Message (Message(messageChannelId))
import Discord.API.Internal.Http.Channel (sendMessage)
import Control.Monad.Reader (asks)
import Discord.Core.Comms (sendText, sendEmbeds)
import Discord.Core.Embeds (runEmbedBuilder, description, title, image, videoHeight, videoWidth, videoProxyUrl)


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
    

customHandler :: BotM ()
customHandler = do
    onCommand "embed" $ \(msg, args) -> do
        let chid = messageChannelId msg
        let embed = runEmbedBuilder $ do 
                        title "A nice title"
                        description "a nicer embed description"
                        image "https://cryptocurrency-nieuws.nl/wp-content/uploads/2022/01/kan-cardano-de-2-halen-deze-week.jpg" $ pure ()

        sendEmbeds chid [ embed ]
        liftIO $ print "Embed sent!"
        liftIO $ print embed

    onCommand "ping" $ \(msg, args) -> do
        let chid = messageChannelId msg
        sendText chid "Pong!"
        liftIO $ print "Pong!"


main :: IO ()
main = do
    startBot app