module PGN where

import Data.Map (Map, toList)
import Data.Text (Text, concat, unlines, unwords)
import Data.Text.Extended (quote)
import TextShow (TextShow (showb), fromText, toText)
import Prelude (($), (.), (<$>), (<>))

data Game = Game
  { headers :: Map Text Text,
    moves :: [Text]
  }

instance TextShow Game where
  showb Game {headers, moves} =
    let headerText = unlines $ (\(k, v) -> concat ["[", k, " ", quote v, "]"]) <$> toList headers
        moveText = unwords moves
     in fromText $ headerText <> "\n" <> moveText <> "\n"

newtype PGN = PGN [Game]

instance TextShow PGN where
  showb (PGN games) = fromText $ unlines (toText . showb <$> games)