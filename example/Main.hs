module Main where

import Discord.Core.Internal.Types (BotConfig (..), BotApp (..), BotM)
import Discord.Core.Bot (startBot)
import Discord.Core.Handlers (onCommand)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Discord.API.Internal.Types.Message (Message(messageChannelId))
import Discord.API.Internal.Http.Channel (sendMessage)
import Control.Monad.Reader (asks)
import Discord.Core.Comms (sendText)


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
    onCommand "help" $ \(msg, args) -> do
        liftIO $ print "Hey, i need help"

    onCommand "ping" $ \(msg, args) -> do
        let chid = messageChannelId msg
        sendText chid "Pong!"
        liftIO $ print "Pong!"


main :: IO ()
main = do
    startBot app