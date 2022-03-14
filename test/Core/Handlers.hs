module Core.Handlers where
import Test.Hspec (describe, it, context, shouldBe, shouldReturn, shouldNotBe, before, before_)
import Discord.Core.Internal.Types (BotConfig(BotConfig, prefix), BotEnv (..), BotApp (BotApp, appConfig, appDefinition, appExceptionHandlers))
import Discord.Core.Handlers (onCommand, onMessage)
import Control.Monad.RWS (modify, MonadIO (liftIO))
import Discord.Core.Internal.BotM ( runBotM, eventHandler )
import Discord.Core.Internal.BotAction (runBotAction_, modifyBotState)
import Utils (simpleMessage)
import Discord.API.Internal.Types.BotEvent (BotEvent(MessageCreate, MessageUpdate))
import Discord.Core.Context (Context(MessageCtx, CommandCtx))
import Control.Concurrent.STM (newTVar, newTVarIO, writeTVar, atomically, readTVarIO)



test = do
    let changeState = (+1)
    let cfg = BotConfig "+" "token"
    let initialState = 0
    let createEnvBefore = before (createEnvironment cfg initialState)
    describe "Handlers" $ do
        context "onMessage" $ do
            let exampleHandler = onMessage $ \_ -> modifyBotState changeState
            let exampleEventHandler = eventHandler cfg exampleHandler

            it "should return a BotM containing a new parser" $ do
                let parsers = runBotM cfg exampleHandler
                length parsers `shouldBe` 1

            createEnvBefore . it "when run, the resulting BotHandlerEventParser returns the given action when the event matches with 'messageCreateParser'" $ \env -> do
                let msg = simpleMessage "some text"
                let (_, action) = exampleEventHandler $ MessageCreate msg
                runBotAction_ env action 
                readTVarIO (envState env) `shouldReturn` changeState initialState

            createEnvBefore . it "when run, the resulting BotHandlerEventParser does not return the given action when the event does not match with 'messageCreateParser'" $ \env -> do
                let msg = simpleMessage "some text"
                let (_, action) = exampleEventHandler $ MessageUpdate msg
                runBotAction_ env action
                readTVarIO (envState env) `shouldReturn` initialState

            it "when run, the resulting BotHandlerEventParser returns MessageCtx when the event matches with 'messageCreateParser'" $ do
                let msg = simpleMessage "some text"
                let (ctx, _) = exampleEventHandler $ MessageCreate msg
                ctx `shouldBe` MessageCtx msg

            it "when run, the resulting BotHandlerEventParser does not return MessageCtx when the event does not match with 'messageCreateParser'" $ do
                let msg = simpleMessage "some text"
                let (ctx, _) = exampleEventHandler $ MessageUpdate msg
                ctx `shouldNotBe` MessageCtx msg


        context "onCommand" $ do
            let commandName = "commandName"
            let prefix' = prefix cfg
            let exampleHandler = onCommand commandName $ \_ _ -> modifyBotState changeState
            let exampleEventHandler = eventHandler cfg exampleHandler

            it "should return a BotM containing a new parser" $ do
                let parsers = runBotM cfg exampleHandler
                length parsers `shouldBe` 1

            createEnvBefore . it "when run, the resulting BotHandlerEventParser returns the given action when the event matches with 'commandParser'" $ \env -> do
                let msg = simpleMessage $ prefix' <> commandName
                let (_, action) = exampleEventHandler $ MessageCreate msg
                runBotAction_ env action 
                readTVarIO (envState env) `shouldReturn` changeState initialState

            createEnvBefore . it "when run, the resulting BotHandlerEventParser does not return the given action when the event does not match with 'commandParser'" $ \env -> do
                let msg = simpleMessage "some text"
                let (_, action) = exampleEventHandler $ MessageUpdate msg
                runBotAction_ env action
                readTVarIO (envState env) `shouldReturn` initialState

            it "when run, the resulting BotHandlerEventParser returns MessageCtx when the event matches with 'commandParser'" $ do
                let msg = simpleMessage $ prefix' <> commandName
                let (ctx, _) = exampleEventHandler $ MessageCreate msg
                ctx `shouldBe` CommandCtx commandName [] msg

            it "when run, the resulting BotHandlerEventParser does not return MessageCtx when the event does not match with 'commandParser'" $ do
                let msg = simpleMessage "some text"
                let (ctx, _) = exampleEventHandler $ MessageCreate msg
                ctx `shouldNotBe` CommandCtx commandName [] msg
    
    where
        createEnvironment cfg initialState = do
            state <- liftIO $ newTVarIO initialState
            waitingTasks <- liftIO $ newTVarIO []
            pure $ BotEnv { envConfig        = cfg
                          , envState         = state
                          , _envWaitingTasks = waitingTasks
                          }