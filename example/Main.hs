module Main where

import Discord.Core.Internal.Types (BotConfig (..), BotApp (..), BotM)
import Discord.Core.Bot (startBot)
import Discord.Core.Handlers (onCommand)
import Control.Monad.IO.Class (MonadIO(liftIO))


customConfig :: BotConfig
customConfig = BotConfig
    { prefix = "?"
    , token = "OTMwODQxNzczMzI3MTU1MjAw.Yd7v9A.txbHnAX0h6d18M3jNbwFSFFiz5Q" }

app :: BotApp
app = BotApp
    { config  = customConfig 
    , handler = customHandler }
    

customHandler :: BotM ()
customHandler = do
    onCommand "help" $ \(msg, args) -> do
        liftIO $ print "Hey, i need help"

    onCommand "ping" $ \(msg, args) -> do
        liftIO $ print "Pong!"


main :: IO ()
main = do
    startBot app