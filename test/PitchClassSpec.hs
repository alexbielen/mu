module PitchClassSpec (spec) where

import           Test.Hspec
import qualified PitchClass as PC

spec :: Spec
spec = do
  spec_orderedInterval

spec_orderedInterval :: SpecWith ()
spec_orderedInterval = describe "interval"
  $ do
    it "returns 4 for 0 4" $ PC.orderedInterval 0 4 `shouldBe` 4
    it "returns 11 for 0 11" $ PC.orderedInterval 0 11 `shouldBe` 11
    it "returns 0 for 2 2" $ PC.orderedInterval 2 2 `shouldBe` 0
    it "returns 11 for 0 1" $ PC.orderedInterval 0 11 `shouldBe` 11
    it "returns 8 for 4 0" $ PC.orderedInterval 4 0 `shouldBe` 8
    it "returns 1 for 11 0" $ PC.orderedInterval 11 0 `shouldBe` 1
