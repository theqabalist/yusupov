module Input where

import Data.Text (Text)
import Data.Yaml (FromJSON, ToJSON)
import Diagram (Diagram)
import Exercise (Exercise)
import GHC.Generics (Generic)
import Prelude (Eq, Int, Show)

data Input = Input
  { book :: Text,
    chapter :: Int,
    diagrams :: [Diagram],
    exercises :: [Exercise]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Input

instance ToJSON Input
