module FENSpec where

import Data.Attoparsec.Text
import Data.Either
import FEN
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "empty" $
    it "is a parser for numbers" $ do
      parseOnly (empty 0) "3" `shouldBe` Right 3
      parseOnly (empty 0) "p" `shouldSatisfy` isLeft
  describe "peice" $
    it "is a parser for peices" $ do
      parseOnly (peice 0) "k" `shouldBe` Right 1
      parseOnly (peice 0) "1" `shouldSatisfy` isLeft
  describe "rankSep" $
    it "is a parser for the rank separator" $ do
      parseOnly (rankSep 8) "/" `shouldBe` Right 8
      parseOnly (rankSep 9) "/" `shouldSatisfy` isLeft
      parseOnly (rankSep 8) "k" `shouldSatisfy` isLeft
  describe "fenBoard" $
    it "is a parser for the fenBoard portion of a fen" $
      parseOnly fenBoard "3b1kr1/3p1p1R/q1rP4/3R2P1/4QP2/PK6/1P6/8 " `shouldBe` Right 64
  describe "validate" $
    it "validates a fen" $ do
      validate (FEN "3b1kr1/3p1p1R/q1rP4/3R2P1/4QP2/PK6/1P6/8 w - - 0 1") `shouldBe` Right ()
      validate (FEN "3b1kr1/3p1p1R/q1rP4/3R2P1/4QP2/PK6/1P6/7 w - - 0 1") `shouldBe` Left "Failed reading: too little data"