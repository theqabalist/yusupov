module Transform where

import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import Data.Map (fromList)
import Data.Text (concat)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeEither')
import qualified Diagram as D
import Exercise (Exercise (difficulty))
import qualified Exercise as E
import Input (Input (..))
import PGN (Game (..), PGN (PGN))
import TextShow (TextShow (showt))
import Prelude hiding (concat)

makeIntro :: Input -> Game
makeIntro Input {book, chapter, name} =
  Game
    { headers =
        fromList
          [ ("White", book),
            ("Black", "Chapter " <> showt chapter),
            ("Event", name),
            ("Site", "?"),
            ("Date", "????.??.??"),
            ("Round", "?")
          ],
      moves = ["*"]
    }

makeResult :: [Exercise] -> Game
makeResult exs =
  let total = sum $ difficulty <$> exs
   in Game
        { headers =
            fromList
              [ ("Event", "<result>"),
                ("Site", "?"),
                ("Date", "????.??.??"),
                ("Round", "?"),
                ("White", "Result"),
                ("Black", concat ["?/", showt total])
              ],
          moves = ["*"]
        }

transform :: ByteString -> ByteString
transform = encodeUtf8 . either (error . show) id . (toPGN <=< (either (Left . (\x -> (show x, show x))) Right . decodeEither'))
  where
    indexed items = zip [1 ..] items
    validateAll Input {diagrams, exercises} = (,) <$> traverse D.validate diagrams <*> traverse (uncurry E.validate) (indexed exercises)
    toPGN input@Input {chapter} = do
      (diagrams, exercises) <- validateAll input
      pure $
        concat $
          showt
            <$> [ PGN [makeIntro input],
                  PGN (D.makeGame chapter <$> diagrams),
                  PGN (uncurry (E.makeGame chapter) <$> indexed exercises),
                  PGN [makeResult exercises]
                ]
