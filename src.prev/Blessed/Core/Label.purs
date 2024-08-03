module Blessed.Core.Label where

import Prelude
import Data.Argonaut.Encode (class EncodeJson, encodeJson)



data Side
    = Left
    | Right



type Label
    = { side :: Side, text :: String }



text :: String -> Label
text t = { side : Left, text : t }


textAt :: String -> Side -> Label
textAt t side = { side, text : t }


instance Show Side where
    show Left = "left"
    show Right = "Right"


instance EncodeJson Side where
    encodeJson side = encodeJson $ show side