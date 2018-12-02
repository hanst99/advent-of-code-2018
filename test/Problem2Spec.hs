module Problem2Spec (spec)
where

import Test.Hspec
import Problem2

spec = do
  describe "neighbor" $ do
    it "should work on one example" $ do
      neighbor "abxcd" "abycd" `shouldBe` Just "abcd"
    it "should fail if the strings aren't the same length" $ do
      neighbor "abxcd" "abycde" `shouldBe` Nothing
    it "should fail if there is no common error in the same spot" $ do
      neighbor "abcd" "abcd" `shouldBe` Nothing
    it "should fail for string with multiple common errors" $ do
      neighbor "axbcyd" "aybcxd" `shouldBe` Nothing
