
import Mastermind
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "arrangements" $ do
    it "should have length 1296" $
      length arrangements `shouldBe` 360
