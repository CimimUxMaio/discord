module Core.Embeds.Spec where
import Test.Hspec (describe, context, it, shouldBe, shouldContain, shouldSatisfy, shouldEndWith)
import Discord.API.Internal.Types.Embed (EmbedAuthor(EmbedAuthor, embedAuthorName, embedAuthorUrl, embedAuthorIconUrl, embedAuthorProxyIconUrl), Embed (..), EmbedThumbnail (embedThumbnailUrl), EmbedField (EmbedField), EmbedImage (embedImageUrl), EmbedFooter (EmbedFooter, embedFooterText, embedFooterIconUrl, embedFooterProxyIconUrl))
import Discord.Core.Embeds.Builder (author, runEmbedBuilder, authorUrl, authorIconUrl, authorProxyIconUrl, title, url, thumbnail, description, field, image, footer, footerIconUrl, footerProxyIconUrl, color, timestamp)
import Discord.Core.Embeds.Colors (red)
import Data.Time (getCurrentTime)


test = describe "Embeds" $ do
    describe "Builder" $ do
        context "author" $ do
            let resultingAuthor = embedAuthor . runEmbedBuilder
            let resultingField getter builder = resultingAuthor builder >>= getter

            it "should add the given author to the embed being created" $ do
                let authorName = "name"
                let expectedAuthor = EmbedAuthor authorName Nothing Nothing Nothing
                let builder = author authorName $ pure ()
                embedAuthor (runEmbedBuilder builder) `shouldBe` Just expectedAuthor

            it "should add the correct name field to the final EmbedAuthor" $ do
                let authorName = "name"
                let builder = author authorName $ pure ()
                resultingField (Just . embedAuthorName) builder `shouldBe` Just authorName
            
            it "authorUrl should add the optional author url field to the final EmbedAuthor" $ do
                let authorUrl' = "author url"
                let builder = author "name" $ authorUrl authorUrl'
                resultingField embedAuthorUrl builder `shouldBe` Just authorUrl'

            it "authorIconUrl should add the optional author icon url field to the final EmbedAuthor" $ do
                let authorIconUrl' = "author icon url"
                let builder = author "name" $ authorIconUrl authorIconUrl'
                resultingField embedAuthorIconUrl builder `shouldBe` Just authorIconUrl'

            it "authorIconProxyUrl should add the optional author icon proxy url field to the final EmbedAuthor" $ do
                let authorIconProxyUrl' = "author icon proxy url"
                let builder = author "name" $ authorProxyIconUrl authorIconProxyUrl'
                resultingField embedAuthorProxyIconUrl builder `shouldBe` Just authorIconProxyUrl'

        context "title" $ do
            it "should add the given title to the embed being created" $ do
                let title' = "title"
                let builder = title title'
                embedTitle (runEmbedBuilder builder) `shouldBe` Just title'

        context "url" $ do
            it "should add the given url to the embed being created" $ do
                let url' = "url"
                let builder = url url'
                embedUrl (runEmbedBuilder builder) `shouldBe` Just url'
            
        context "thumbnail" $ do
            let resultingThumbnail = embedThumbnail . runEmbedBuilder
            let resultingField getter builder = resultingThumbnail builder >>= getter

            it "should add the given thumbnail to the embed being created" $ do
                let thumbnail' = "thumbnail url"
                let builder = thumbnail thumbnail'
                resultingField (Just . embedThumbnailUrl) builder `shouldBe` Just thumbnail'
            
        context "description" $ do
            it "should add the given description to the embed being created" $ do
                let description' = "description"
                let builder = description description'
                embedDescription (runEmbedBuilder builder) `shouldBe` Just description'
            

        context "field" $ do
            it "should add an EmbedField to the field list of the embed being created" $ do
                let builder = field "name" "value" False
                let expectedField = EmbedField "name" "value" False
                embedFields (runEmbedBuilder builder) `shouldContain` [expectedField]

            it "the resulting embed field list should be empty if 'field' is not used" $ do
                let builder = pure ()
                embedFields (runEmbedBuilder builder) `shouldSatisfy` null

            it "should add EmbedFields in the given order" $ do
                let field2@(EmbedField name2 value2 inline2) = EmbedField "name2" "value2" False
                let builder = field "name1" "value1" False >> field name2 value2 inline2
                embedFields (runEmbedBuilder builder) `shouldEndWith` [field2]


        context "image" $ do
            let resultingImage = embedImage . runEmbedBuilder
            let resultingField getter builder = resultingImage builder >>= getter

            it "should add the given image to the embed being created" $ do
                let imageUrl = "image url"
                let builder = image imageUrl
                resultingField (Just . embedImageUrl) builder `shouldBe` Just imageUrl
            
    
        context "footer" $ do
            let resultingFooter = embedFooter . runEmbedBuilder
            let resultingField getter builder = resultingFooter builder >>= getter

            it "should add the given footer to the embed being created" $ do
                let footerText = "text"
                let expectedFooter = EmbedFooter footerText Nothing Nothing
                let builder = footer footerText $ pure ()
                embedFooter (runEmbedBuilder builder) `shouldBe` Just expectedFooter

            it "should add the correct text field to the final EmbedFooter" $ do
                let footerText = "text"
                let builder = footer footerText $ pure ()
                resultingField (Just . embedFooterText) builder `shouldBe` Just footerText
            
            it "footerIconUrl should add the optional icon url field to the final EmbedFooter" $ do
                let footerIconUrl' = "footer icon url"
                let builder = footer "text" $ footerIconUrl footerIconUrl'
                resultingField embedFooterIconUrl builder `shouldBe` Just footerIconUrl'

            it "footerIconProxyUrl should add the optional icon proxy url field to the final EmbedFooter" $ do
                let footerProxyIconUrl' = "footer icon proxy url"
                let builder = footer "text" $ footerProxyIconUrl footerProxyIconUrl'
                resultingField embedFooterProxyIconUrl builder `shouldBe` Just footerProxyIconUrl'

        context "color" $ do
            it "should add the given EmbedColor to the embed being created" $ do
                let color' = red
                let builder = color color'
                embedColor (runEmbedBuilder builder) `shouldBe` Just color'
            
        context "timestamp" $ do
            it "should add the given UTCTime to the embed being created" $ do
                time <- getCurrentTime
                let builder = timestamp time
                embedTimestamp (runEmbedBuilder builder) `shouldBe` Just time
