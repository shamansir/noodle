module Blessed.Core.Terminal where

import Prelude


import Data.Argonaut.Encode (class EncodeJson, encodeJson)

data Terminal
    = XTerm
    | WinAnsi
    -- | TODO


instance EncodeJson Terminal where
    encodeJson XTerm = encodeJson "xterm"
    encodeJson WinAnsi = encodeJson "windows-ansi"