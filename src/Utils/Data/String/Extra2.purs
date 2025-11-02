module Data.String.Extra2 where

import Prelude

import Data.String (split, Pattern(..)) as String
import Data.Array (length) as Array


lines :: String -> Array String
lines = String.split (String.Pattern "\n")


linesCount :: String -> Int
linesCount = Array.length <<< lines


-- https://github.com/purescript-halogen/purescript-halogen-vdom-string-renderer/blob/master/src/Halogen/VDom/StringRenderer/Util.purs
foreign import escapeHtml :: String -> String
