module PitchClassSpec (spec) where

import           Test.Hspec
import qualified PitchClass as PC

spec :: Spec
spec = do
  spec_orderedInterval
  spec_unorderedInterval

spec_orderedInterval :: SpecWith ()
spec_orderedInterval = describe "orderedInterval"
  $ do
    it "returns 4 for 0 4" $ PC.orderedInterval 0 4 `shouldBe` 4
    it "returns 11 for 0 11" $ PC.orderedInterval 0 11 `shouldBe` 11
    it "returns 0 for 2 2" $ PC.orderedInterval 2 2 `shouldBe` 0
    it "returns 11 for 0 1" $ PC.orderedInterval 0 11 `shouldBe` 11
    it "returns 8 for 4 0" $ PC.orderedInterval 4 0 `shouldBe` 8
    it "returns 1 for 11 0" $ PC.orderedInterval 11 0 `shouldBe` 1

spec_unorderedInterval :: SpecWith ()
spec_unorderedInterval = describe "unorderedInterval"
  $ do
    it "returns 4 for 0 4" $ PC.unorderedInterval 0 4 `shouldBe` 4
    it "returns 1 for 0 11" $ PC.unorderedInterval 0 11 `shouldBe` 1
    it "returns 1 for 11 0" $ PC.unorderedInterval 11 0 `shouldBe` 1
    it "returns 0 for 3 3" $ PC.unorderedInterval 3 3 `shouldBe` 0
