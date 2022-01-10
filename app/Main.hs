module Main where

import Data.ByteString (interact)
import Transform (transform)
import Prelude (IO)

main :: IO ()
main = interact transform
