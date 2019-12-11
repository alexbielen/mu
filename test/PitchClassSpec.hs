module PitchClassSpec (spec) where

import           Test.Hspec
import qualified PitchClass as PC

spec :: Spec
spec = do
  testInterval

testInterval :: SpecWith ()
testInterval = describe "interval"
  $ do
    it "returns Maj3 for C E"
      $ PC.ascendingInterval PC.C PC.E `shouldBe` Just PC.Maj3
    it "returns Min2 for B C"
      $ PC.ascendingInterval PC.B PC.C `shouldBe` Just PC.Min2
    it "returns a unison for D D"
      $ PC.ascendingInterval PC.D PC.D `shouldBe` Just PC.Unison
