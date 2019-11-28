import           Test.Hspec
import qualified Lib                            ( FitMode(..)
                                                , fit
                                                )

main :: IO ()
main = hspec $ describe "fit" $ do
    it "returns n when n is in between min and max and mode is Wrap"
        $          Lib.fit 2 1 3 Lib.Wrap
        `shouldBe` 2

    it "returns n when n is in between min and max and mode is Reflect"
        $          Lib.fit 2 1 3 Lib.Reflect
        `shouldBe` 2

    it "returns n when n is in between min and max and mode is Range"
        $          Lib.fit 2 1 3 Lib.Range
        `shouldBe` 2

    it "returns min when n is less than min and mode is Range"
        $          Lib.fit (-1) 1 3 Lib.Range
        `shouldBe` 1

    it "returns max when n is greater than max and mode is Range"
        $          Lib.fit 4 1 3 Lib.Range
        `shouldBe` 3
