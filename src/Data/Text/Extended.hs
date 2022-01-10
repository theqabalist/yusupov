module Data.Text.Extended where

import Data.Text (Text, concat)

quote :: Text -> Text
quote t = concat ["\"", t, "\""]