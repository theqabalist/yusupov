module FENSpec where

import Data.Attoparsec.Text
import Data.Either
import FEN
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "fenRow" $
    it "is a parser for a board row" $ do
      parseOnly fenRow "8" `shouldBe` Right "8"
      parseOnly fenRow "1k" `shouldBe` Right "1k6"
      parseOnly fenRow "3" `shouldBe` Right "35"
      parseOnly fenRow "3" `shouldBe` Right "35"
      parseOnly fenRow "rnbqkbnr" `shouldBe` Right "rnbqkbnr"
      parseOnly fenRow "rnbqkbn" `shouldBe` Right "rnbqkbn1"
  describe "fenBoard" $
    it "is a parser for a board" $ do
      parseOnly fenBoard "///////" `shouldBe` Right "8/8/8/8/8/8/8/8"
      parseOnly fenBoard "1k" `shouldBe` Right "1k6/8/8/8/8/8/8/8"
      parseOnly fenBoard "3b1kr/3p1p1R//////1k" `shouldBe` Right "3b1kr1/3p1p1R/8/8/8/8/8/1k6"
  describe "fen" $
    it "is a parser for an auto-completing fen" $ do
      parseOnly fen "3b1kr1/3p1p1R/q1rP4/3R2P1/4QP2/PK6/1P6/8 w - - 0 1" `shouldBe` Right (FEN "3b1kr1/3p1p1R/q1rP4/3R2P1/4QP2/PK6/1P6/8 w - - 0 1")
      parseOnly fen "3b1kr/3p1p1R/q1rP/3R2P/4QP/PK/1P/8 w - - 0 1" `shouldBe` Right (FEN "3b1kr1/3p1p1R/q1rP4/3R2P1/4QP2/PK6/1P6/8 w - - 0 1")
      parseOnly fen "3b1kr/3p1p1R w - - 0 1" `shouldBe` Right (FEN "3b1kr1/3p1p1R/8/8/8/8/8/8 w - - 0 1")
      parseOnly fen "3b1kr/3p1p1R" `shouldBe` Right (FEN "3b1kr1/3p1p1R/8/8/8/8/8/8 w - - 0 1")
      parseOnly fen "3b1kr/3p1p1R b" `shouldBe` Right (FEN "3b1kr1/3p1p1R/8/8/8/8/8/8 b - - 0 1")
      parseOnly fen "3b1kr/3p1p1R Kq" `shouldBe` Right (FEN "3b1kr1/3p1p1R/8/8/8/8/8/8 w Kq - 0 1")
      parseOnly fen "3b1kr/3p1p1R//////1k w - - 0 1" `shouldBe` Right (FEN "3b1kr1/3p1p1R/8/8/8/8/8/1k6 w - - 0 1")