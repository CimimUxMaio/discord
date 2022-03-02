module Core.Internal.Spec where


import Test.Hspec (describe)
import qualified Core.Internal.Parsers as Parsers


test = describe "Internal" $ do
    Parsers.test