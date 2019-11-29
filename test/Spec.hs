import           Test.Hspec
import qualified Lib                            ( FitMode(..)
                                                , fit
                                                )

main :: IO ()
main = hspec $ describe "fit" $ do
    let wrapped   = Lib.fit Lib.Wrap
    let reflected = Lib.fit Lib.Reflect
    let limited   = Lib.fit Lib.Limit
    -- Wrap Mode
    it "returns n when n is in between min and max and mode is Wrap"
        $          wrapped 1 3 2
        `shouldBe` 2

    it "returns a wrapped value when n is greater than max and mode is Wrap"
        $          wrapped 0 6 7
        `shouldBe` 1

    it "returns a wrapped value when n is less than min and mode is Wrap"
        $          wrapped 0 6 (-1)
        `shouldBe` 5

    -- Reflect Mode
    it "returns n when n is in between min and max and mode is Reflect"
        $          reflected 1 3 2
        `shouldBe` 2

    -- Limit mode
    it "returns n when n is in between min and max and mode is Range"
        $          limited 1 3 2
        `shouldBe` 2

    it "returns min when n is less than min and mode is Range"
        $          limited 1 3 (-1)
        `shouldBe` 1

    it "returns max when n is greater than max and mode is Range"
        $          limited 1 3 4
        `shouldBe` 3
