module FEN where

import Control.Applicative
  ( Alternative (many, (<|>)),
  )
import Data.Attoparsec.Text hiding (take)
import Data.Char (digitToInt)
import Data.Functor
import Data.List (intercalate)
import Data.Text hiding (empty, foldr, intercalate, length, replicate, singleton)
import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)
import TextShow (TextShow (showb), fromText)
import Prelude

newtype FEN = FEN Text
  deriving (Eq, Show, Generic)

instance ToJSON FEN

instance FromJSON FEN

instance TextShow FEN where
  showb (FEN f) = fromText f

fenMove :: Parser String
fenMove = (: []) <$> satisfy (inClass "wb")

fenCastling :: Parser String
fenCastling = ((: []) <$> char '-') <|> many (satisfy (inClass "KQkq"))

fenEnPassant :: Parser String
fenEnPassant = ((: []) <$> char '-') <|> ((\file rank -> [file, rank]) <$> satisfy (inClass "a-h") <*> satisfy (inClass "1-8"))

fenHalfMoveClock :: Parser String
fenHalfMoveClock = show <$> (decimal :: Parser Int)

fenFullMove :: Parser String
fenFullMove = show <$> (decimal :: Parser Int)

explicitEmpty :: Parser Char
explicitEmpty = satisfy (inClass "1-8")

peice2 :: Parser Char
peice2 = satisfy (inClass "pnbrqk") <|> satisfy (inClass "PNBRQK")

autocompleteRow :: String -> Parser String
autocompleteRow "" = pure "8"
autocompleteRow row =
  let total = foldr (\e t -> t + if inClass "1-8" e then digitToInt e else 1) 0 row
   in if total < 8
        then pure (row ++ show (8 - total))
        else
          if total == 8
            then pure row
            else fail ("Too many items in row '" ++ row ++ "'")

fenRow :: Parser String
fenRow = many (explicitEmpty <|> peice2) >>= autocompleteRow

fenBoard' :: Parser [String]
fenBoard' = do
  rows <- sepBy fenRow (string "/")
  let n = length rows
  if n < 8
    then pure $ rows ++ replicate (8 - n) "8"
    else
      if n == 8
        then pure rows
        else fail "Too many rows on board"

fenBoard :: Parser String
fenBoard = intercalate "/" <$> fenBoard'

spaceS :: Parser String
spaceS = (: []) <$> space

fen :: Parser FEN
fen = FEN . pack . intercalate "" <$> sequence [fenBoard, spaceS, fenMove, spaceS, fenCastling, spaceS, fenEnPassant, spaceS, fenHalfMoveClock, spaceS, fenFullMove]

validate :: FEN -> Either String FEN
validate (FEN textFEN) = parseOnly fen textFEN