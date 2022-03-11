module Discord.Core.Internal.BotAction where

import Discord.Core.Internal.Types (BotEnv (..), BotAction (_runBotAction), BotConfig)
import Control.Monad (void)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.RWS (asks, MonadIO (liftIO))
import Control.Concurrent.STM (readTVarIO, atomically, modifyTVar)


runBotAction :: BotEnv s -> BotAction s a -> IO a
runBotAction env = (`runReaderT` env) . _runBotAction

runBotAction_ :: BotEnv s -> BotAction s a -> IO ()
runBotAction_ env = void . runBotAction env



{- Helper functions -}

-- | Helper for getting the bot's 'BotConfig' being used.
getBotConfig :: BotAction s BotConfig
getBotConfig = asks envConfig

-- | Helper for reading the bot's current state.
readBotState :: BotAction s s
readBotState = asks envState >>= liftIO . readTVarIO

-- | Helper for modifying the bot's state.
modifyBotState :: (s -> s)  -- ^ transformation
               -> BotAction s ()
modifyBotState f = asks envState >>= liftIO . atomically . (`modifyTVar` f)

-- | Helper for setting the bot's state.
setBotState :: s  -- ^ new state
            -> BotAction s ()
setBotState = modifyBotState . const