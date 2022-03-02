module Main where
import Test.Hspec (hspec)
import qualified Core.Spec as Core

main :: IO ()
main = hspec $ do
    Core.test

