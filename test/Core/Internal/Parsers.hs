module Core.Internal.Parsers where
import Test.Hspec (describe, shouldSatisfy, it, shouldNotSatisfy, shouldBe, context)
import Discord.API.Internal.Types.Message (Message(Message))
import Discord.API.Internal.Types.Common (Snowflake(Snowflake))
import Discord.API.Internal.Types.User (User(User))
import Utils (simpleMessage)
import Discord.API.Internal.Types.BotEvent (BotEvent(MessageCreate, MessageUpdate))
import Discord.Core.Internal.Types (runBotEventParser)
import Discord.Core.Internal.Parsers (messageCreateParser, commandParser)
import Data.Maybe (isJust)
import Discord.Core.Context (Context(MessageCtx, CommandCtx))


test = describe "Parsers" $ do
    context "messageCreateParser" $ do
        let runParser = runBotEventParser messageCreateParser
        let sampleMessage = simpleMessage "some message content"
        it "succeeds when run with a MessageCreate BotEvent" $ do
            let event = MessageCreate sampleMessage
            runParser event `shouldSatisfy` isJust

        it "fails when run with a BotEvent other than MessageCreate" $ do
            let event = MessageUpdate sampleMessage
            runParser event `shouldNotSatisfy` isJust

        it "returns the correct MessageCtx on success" $ do
            let event = MessageCreate sampleMessage
            let expectedCtx = MessageCtx sampleMessage
            runParser event `shouldBe` Just expectedCtx

    context "commandParser" $ do
        let prefix = "+"
        let notPrefix = "?"
        let runParser = runBotEventParser . commandParser prefix
        it "succeeds when run with a MessageCreate BotEvent whose content first word starts with the correct prefix and a valid command name" $ do
            let event = MessageCreate . simpleMessage $ prefix <> "commandName"
            runParser "commandName" event `shouldSatisfy` isJust

        it "fails when run with a MessageCreate BotEvent whose content first word does not start with the correct prefix" $ do
            let event = MessageCreate . simpleMessage $ notPrefix <> "commandName"
            runParser "commandName" event `shouldNotSatisfy` isJust
        
        it "fails when run with a MessageCreate BotEvent whose content first word starts with the correct prefix but does not follow with a valid command name" $ do
            let event = MessageCreate . simpleMessage $ prefix <> "invalidCommandName"
            runParser "commandName" event `shouldNotSatisfy` isJust

        it "fails when run with a BotEvent other than a MessageCreate" $ do
            let event = MessageUpdate . simpleMessage $ prefix <> "commandName"
            runParser "commandName" event `shouldNotSatisfy` isJust

        it "returns the correct CommandCtx on success" $ do
            let args = "arg1 arg2 arg3"
            let commandName = "commandName"
            let msg = simpleMessage $ prefix <> commandName <> " arg1 arg2 arg3"
            let event = MessageCreate msg
            let expectedCtx = CommandCtx commandName ["arg1", "arg2", "arg3"] msg
            runParser commandName event `shouldBe` Just expectedCtx