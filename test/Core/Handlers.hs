module Core.Handlers where
import Test.Hspec (describe, it, context, shouldBe, shouldReturn, shouldNotBe)
import Discord.Core.Internal.Types (BotConfig(BotConfig, prefix), runBotM, runBotAction_)
import Discord.Core.Handlers (onCommand, onMessage)
import Control.Monad.RWS (modify)
import Discord.Core.Internal.Utils (eventHandler)
import Utils (simpleMessage)
import Discord.API.Internal.Types.BotEvent (BotEvent(MessageCreate, MessageUpdate))
import Discord.Core.Context (Context(MessageCtx, CommandCtx))



test = describe "Handlers" $ do
    let cfg = BotConfig "+" "token"
    let initialState = 0
    let changeState = (+1)
    let runAction = runBotAction_ cfg initialState

    context "onMessage" $ do
        let exampleHandler = onMessage $ \_ -> modify changeState
        let exampleEventHandler = eventHandler cfg exampleHandler

        it "should return a BotM containing a new parser" $ do
            let parsers = runBotM exampleHandler cfg
            length parsers `shouldBe` 1

        it "when run, the resulting BotHandlerEventParser returns the given action when the event matches with 'messageCreateParser'" $ do
            let msg = simpleMessage "some text"
            let (_, action) = exampleEventHandler $ MessageCreate msg
            runAction action `shouldReturn` changeState initialState

        it "when run, the resulting BotHandlerEventParser does not return the given action when the event does not match with 'messageCreateParser'" $ do
            let msg = simpleMessage "some text"
            let (_, action) = exampleEventHandler $ MessageUpdate msg
            runAction action `shouldReturn` initialState

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
        let exampleHandler = onCommand commandName $ \_ _ -> modify changeState
        let exampleEventHandler = eventHandler cfg exampleHandler

        it "should return a BotM containing a new parser" $ do
            let parsers = runBotM exampleHandler cfg
            length parsers `shouldBe` 1

        it "when run, the resulting BotHandlerEventParser returns the given action when the event matches with 'commandParser'" $ do
            let msg = simpleMessage $ prefix' <> commandName
            let (_, action) = exampleEventHandler $ MessageCreate msg
            runAction action `shouldReturn` changeState initialState
        
        it "when run, the resulting BotHandlerEventParser does not return the given action when the event does not match with 'commandParser'" $ do
            let msg = simpleMessage "some text"
            let (_, action) = exampleEventHandler $ MessageUpdate msg
            runAction action `shouldReturn` initialState

        it "when run, the resulting BotHandlerEventParser returns MessageCtx when the event matches with 'commandParser'" $ do
            let msg = simpleMessage $ prefix' <> commandName
            let (ctx, _) = exampleEventHandler $ MessageCreate msg
            ctx `shouldBe` CommandCtx commandName [] msg
        
        it "when run, the resulting BotHandlerEventParser does not return MessageCtx when the event does not match with 'commandParser'" $ do
            let msg = simpleMessage "some text"
            let (ctx, _) = exampleEventHandler $ MessageCreate msg
            ctx `shouldNotBe` CommandCtx commandName [] msg