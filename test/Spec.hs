import           Test.Hspec
import qualified Lib                            ( FitMode(..)
                                                , fit
                                                , fitF
                                                )


main :: IO ()
main = do
    testFit
    testFitF

testFit :: IO ()
testFit = hspec $ describe "fit" $ do
    let wrapped = Lib.fit Lib.Wrap
    let clamped = Lib.fit Lib.Clamp
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

    -- Limit mode
    it "returns n when n is in between min and max and mode is Range"
        $          clamped 1 3 2
        `shouldBe` 2

    it "returns min when n is less than min and mode is Range"
        $          clamped 1 3 (-1)
        `shouldBe` 1

    it "returns max when n is greater than max and mode is Range"
        $          clamped 1 3 4
        `shouldBe` 3

testFitF :: IO ()
testFitF = hspec $ describe "fitF" $ do
    -- Wrap Mode 
    it "returns n in a list when n is between min and max and mode is Wrap"
        $          Lib.fitF Lib.Wrap 1 3 [2]
        `shouldBe` [2]

    it "returns n in a Just when n is between min and max and mode is Wrap"
        $          Lib.fitF Lib.Wrap 1 3 (Just 2)
        `shouldBe` Just 2






