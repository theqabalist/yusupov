module Diagram where

import Data.Map (fromList)
import Data.Text (Text, concat)
import Data.Yaml (FromJSON, ToJSON)
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

instance FromJSON Diagram