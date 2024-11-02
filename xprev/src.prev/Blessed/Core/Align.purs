module Blessed.Core.Align where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Codec.Argonaut as CA



data HAlign
    = Left
    | Center
    | Right


data VAlign
    = Top
    | Middle
    | Bottom


instance EncodeJson HAlign where
    encodeJson = CA.encode CA.string <<< show


instance Show HAlign where
    show Left = "left"
    show Center = "center"
    show Right = "right"


instance EncodeJson VAlign where
    encodeJson = CA.encode CA.string <<< show


instance Show VAlign where
    show Top = "top"
    show Middle = "middle"
    show Bottom = "bottom"
