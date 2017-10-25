
import Mastermind
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "arrangements" $ do
    it "should have length 1296" $
      length arrangements `shouldBe` 360

  describe "points" $ do
    it "returns [4,0] when the guess is correct" $ do
      points [1,2,3,4] [1,2,3,4] `shouldBe` [4,0]

    it "returns [0,4] when the guess is the reversed answer" $
      let answer = [1,2,3,4]
          guess = reverse answer
      in do points answer guess `shouldBe` [0,4]

    it "returns [2,2] when then guess has 2 corrects and 2 regulars" $
      let answer = [4,5,6,1]
          guess  = [6,5,4,1]
      in do points answer guess `shouldBe` [2,2]

    it "returns [0,3] when then guess has 1 correct and 3 regulars" $
      let answer = [5,1,6,3]
          guess  = [2,5,3,1]
      in do points answer guess `shouldBe` [0,3]

  describe "pool" $ do

    it "returns the desired new pool" $
      let pool = arrangements
      in do (updatePool pool [1,2,3,4] 0 4)
        `shouldBe` [[2,1,4,3], [2,3,4,1], [2,4,1,3], [3,1,4,2], [3,4,1,2],
                    [3,4,2,1], [4,1,2,3], [4,3,1,2], [4,3,2,1]]
