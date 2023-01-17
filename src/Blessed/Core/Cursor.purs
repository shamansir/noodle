module Blessed.Core.Cursor where

import Prelude

import Blessed.Core.Color (Color)

import Data.Argonaut.Encode (class EncodeJson, encodeJson)

data Shape
    = Block
    | Underline
    | Line


type Cursor =
    { artificial :: Boolean
    , blink :: Boolean
    , shape :: Shape
    , color :: Color
    }


instance EncodeJson Shape where
    encodeJson Block = encodeJson "block"
    encodeJson Underline = encodeJson "underline"
    encodeJson Line = encodeJson "line"