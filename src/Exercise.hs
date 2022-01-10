module Exercise where

import Data.Map (fromList)
import Data.Text (concat, replicate)
import Data.Yaml (FromJSON, ToJSON)
import qualified FEN
import GHC.Generics (Generic)
import PGN (Game (..))
import TextShow (TextShow (showt))
import Prelude hiding (concat, replicate)

data Exercise = Exercise
  { difficulty :: Int,
    fen :: FEN.FEN
  }
  deriving (Eq, Show, Generic)

instance ToJSON Exercise

instance FromJSON Exercise

makeGame :: Int -> Int -> Exercise -> Game
makeGame chapter idx ex =
  Game
    { headers =
        fromList
          [ ("Event", "?"),
            ("Site", "?"),
            ("Date", "????.??.??"),
            ("Round", "?"),
            ("White", concat ["Exercise ", showt chapter, ".", showt idx]),
            ("Black", replicate (difficulty ex) "★"),
            ("Result", "*"),
            ("FEN", showt $ fen ex),
            ("SetUp", showt (1 :: Int))
          ],
      moves = ["*"]
    }

validate :: Int -> Exercise -> Either (String, String) Exercise
validate idx ex@Exercise {fen} = either (Left . (,"Exercise " <> show idx) . fst) (Right . const ex) $ FEN.validate fen