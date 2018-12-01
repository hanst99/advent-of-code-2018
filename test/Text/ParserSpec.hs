{-# LANGUAGE OverloadedStrings #-}
module Text.ParserSpec (spec)
where

import Test.Hspec
import Text.Parser

spec :: Spec
spec = do
  describe "string" $ do
    it "should succeed if the prefix matches" $ do
      runParser (string "hello") "hello world"
        `shouldBe` Just ("hello", " world")
    it "should fail if the prefix doesn't match" $ do
      runParser (string "bye") "hello world"
        `shouldBe` Nothing
