module Diagram where

import Data.Map (fromList)
import Data.Maybe (fromMaybe)
import Data.Text (Text, concat, pack)
import Data.Yaml (FromJSON (parseJSON), Parser, ToJSON, Value (..), (.:), (.:?))
import qualified FEN
import GHC.Generics (Generic)
import PGN (Game (..))
import TextShow (TextShow (showt))
import Prelude hiding (concat)

data Diagram = Diagram
  { designator :: Int,
    white :: Text,
    black :: Text,
    location :: Text,
    date :: Text,
    fen :: FEN.FEN
  }
  deriving (Eq, Show, Generic)

textWithDefault :: Text -> Value -> Parser Text
textWithDefault t Null = pure t
textWithDefault _ v = parseJSON v

parseLocation :: Value -> Parser Text
parseLocation Null = pure ""
parseLocation v = parseJSON v

parseDate :: Value -> Parser Text
parseDate (Number x) = pure $ pack $ show x
parseDate v = parseJSON v

instance FromJSON Diagram where
  parseJSON v = do
    obj <- parseJSON v
    designator <- obj .: "designator"
    white <- obj .:? "white" >>= (textWithDefault "" . fromMaybe "")
    black <- obj .:? "black" >>= (textWithDefault "" . fromMaybe "")
    location <- obj .:? "location" >>= (textWithDefault "" . fromMaybe "")
    date <- obj .:? "date" >>= (parseDate . fromMaybe "")
    fen <- obj .: "fen"
    pure $
      Diagram
        { designator,
          white,
          black,
          location,
          date,
          fen
        }

makeGame :: Int -> Diagram -> Game
makeGame chapter diagram =
  Game
    { headers =
        fromList
          [ ("Event", concat ["Diagram ", showt chapter, ".", showt $ designator diagram]),
            ("Site", location diagram),
            ("Date", date diagram),
            ("Round", "?"),
            ("White", white diagram),
            ("Black", black diagram),
            ("Result", "*"),
            ("FEN", showt $ fen diagram),
            ("SetUp", showt (1 :: Int))
          ],
      moves = ["*"]
    }

validate :: Diagram -> Either (String, String) Diagram
validate dg@Diagram {fen} = either (Left . (,"Diagram " <> show (designator dg)) . fst) (Right . const dg) $ FEN.validate fen

instance ToJSON Diagram