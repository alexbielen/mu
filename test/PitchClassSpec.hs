module PitchClassSpec (spec) where

import           Test.Hspec
import qualified PitchClass as PC

spec :: Spec
spec = do
  spec_orderedInterval

spec_orderedInterval :: SpecWith ()
spec_orderedInterval = describe "interval"
  $ context "when mode is as Asc"
  $ do
    it "returns Maj3 for C E in Asc mode"
      $ PC.orderedInterval PC.Asc PC.C PC.E `shouldBe` PC.Maj3
    it "returns Min2 for B C in Asc mode"
      $ PC.orderedInterval PC.Asc PC.B PC.C `shouldBe` PC.Min2
    it "returns a unison for D D in Asc mode"
      $ PC.orderedInterval PC.Asc PC.D PC.D `shouldBe` PC.Unison
    it "returns a Maj 7 for C B in Asc mode"
      $ PC.orderedInterval PC.Asc PC.C PC.B `shouldBe` PC.Maj7
    it "returns a Min6 for C E in Desc mode"
      $ PC.orderedInterval PC.Desc PC.C PC.E `shouldBe` PC.Min6
    it "returns a Maj7 for B C in Desc mode"
      $ PC.orderedInterval PC.Desc PC.B PC.C `shouldBe` PC.Maj7
