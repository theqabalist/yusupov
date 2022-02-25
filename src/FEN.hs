module FEN where

import Control.Applicative
  ( Alternative (many, (<|>)),
  )
import Data.Attoparsec.Text hiding (take)
import Data.Char (digitToInt)
import Data.Functor
import Data.List (intercalate)
import Data.Text hiding (empty, foldr, intercalate, length, replicate, singleton, unwords)
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

spaceS :: Parser String
spaceS = (: []) <$> space

fenMove :: Parser String
fenMove = (spaceS *> ((: []) <$> satisfy (inClass "wb"))) <|> pure "w"

hyphen :: Parser String
hyphen = (: []) <$> char '-'

fenCastling :: Parser String
fenCastling = (spaceS *> (hyphen <|> many (satisfy (inClass "KQkq")))) <|> pure "-"

fenEnPassant :: Parser String
fenEnPassant = (spaceS *> (hyphen <|> ((\file rank -> [file, rank]) <$> satisfy (inClass "a-h") <*> satisfy (inClass "1-8")))) <|> pure "-"

fenHalfMoveClock :: Parser String
fenHalfMoveClock = (spaceS *> (show <$> (decimal :: Parser Int))) <|> pure "0"

fenFullMove :: Parser String
fenFullMove = (spaceS *> (show <$> (decimal :: Parser Int))) <|> pure "1"

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

fen :: Parser FEN
fen = FEN . pack . unwords <$> sequence [fenBoard, fenMove, fenCastling, fenEnPassant, fenHalfMoveClock, fenFullMove]

validate :: FEN -> Either String FEN
validate (FEN textFEN) = parseOnly fen textFEN