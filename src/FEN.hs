module FEN where

import Control.Applicative hiding (empty)
import Data.Attoparsec.Text
import Data.Char (digitToInt)
import Data.Functor
import Data.Text hiding (empty, singleton)
import Data.Yaml (FromJSON, ToJSON)
import Debug.Trace
import GHC.Generics (Generic)
import TextShow (TextShow (showb), fromText)
import Prelude

newtype FEN = FEN Text
  deriving (Eq, Show, Generic)

instance ToJSON FEN

instance FromJSON FEN

instance TextShow FEN where
  showb (FEN f) = fromText f

empty :: Int -> Parser Int
empty total = (+ total) . digitToInt <$> satisfy (inClass "1-8")

peice :: Int -> Parser Int
peice total = total + 1 <$ (satisfy (inClass "pnbrqk") <|> satisfy (inClass "PNBRQK"))

rankSep :: Int -> Parser Int
rankSep total =
  char '/'
    *> if total `mod` 8 /= 0 then fail message else pure total
  where
    message =
      let ranks :: Int = floor $ (fromIntegral total :: Double) / 8
       in "error after rank " <> show ranks

end :: Int -> Parser Int
end total = space *> calced
  where
    calced = case traceShowId total of
      _ | total < 64 -> fail "too little data"
      _ | total > 64 -> fail "too much data"
      _ | otherwise -> pure total

fenBoardItem :: Int -> Parser Int
fenBoardItem total = end total <|> ((empty total <|> peice total <|> rankSep total <|> end total) >>= fenBoardItem)

fenBoard :: Parser Int
fenBoard = fenBoardItem 0

fenMove :: Parser Char
fenMove = satisfy (inClass "wb")

fenCastling :: Parser String
fenCastling = ((: []) <$> char '-') <|> many (satisfy (inClass "KQkq"))

fenEnPassant :: Parser String
fenEnPassant = ((: []) <$> char '-') <|> ((\file rank -> [file, rank]) <$> satisfy (inClass "a-h") <*> satisfy (inClass "1-8"))

fenHalfMoveClock :: Parser Int
fenHalfMoveClock = decimal

fenFullMove :: Parser Int
fenFullMove = decimal

fen :: Parser ()
fen =
  fenBoard
    *> fenMove
    *> space
    *> fenCastling
    *> space
    *> fenEnPassant
    *> space
    *> fenHalfMoveClock
    *> space
    *> fenFullMove
    $> ()

validate :: FEN -> Either (String, FEN) ()
validate f@(FEN textFEN) = either (Left . (,f)) Right $ parseOnly fen textFEN