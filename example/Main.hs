module Main where

import Discord.Core.Internal.Types (BotConfig (..), BotApp (..), BotM)
import Discord.Core.Bot (startBot)
import Discord.Core.Handlers (onCommand)
import Control.Monad.IO.Class (MonadIO(liftIO))


customConfig :: BotConfig
customConfig = BotConfig
    { prefix = "+"
    , token = "<your_token>" }

app :: BotApp
app = BotApp
    { config  = customConfig 
    , handler = customHandler }
    

customHandler :: BotM ()
customHandler = do
    onCommand "help" $ \args -> do
        liftIO $ print "Hey, i need help"

    onCommand "ping" $ \args -> do
        liftIO $ print "Pong!"


main :: IO ()
main = do
    startBot app