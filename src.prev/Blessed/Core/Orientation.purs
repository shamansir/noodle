module Blessed.Core.Orientation where

import Prelude



import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Codec.Argonaut as CA


data Orientation
    = Horizontal
    | Vertical


instance Show Orientation where
    show Horizontal = "horizontal"
    show Vertical = "vertical"


instance EncodeJson Orientation where
    encodeJson x = CA.encode CA.string $ show x


horizontal :: Orientation
horizontal = Horizontal


vertical :: Orientation
vertical = Vertical


render ∷ Orientation → String
render Horizontal = "horizontal"
render Vertical = "vertical"