module Core.Spec where

import Test.Hspec (describe)
import qualified Core.Internal.Spec as Internal
import qualified Core.Handlers as Handlers
import qualified Core.Embeds.Spec as Embeds


test = describe "Core" $ do
    Handlers.test
    Embeds.test
    Internal.test