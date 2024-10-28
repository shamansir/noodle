module Data.String.Extra2 where

import Prelude

import Data.String (split, Pattern(..)) as String
import Data.Array (length) as Array


lines :: String -> Array String
lines = String.split (String.Pattern "\n")


linesCount :: String -> Int
linesCount = Array.length <<< lines