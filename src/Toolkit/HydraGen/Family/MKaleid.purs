module MKaleid where

import Prelude

_in_what = Fn.Input :: _ "what"
_in_nSides = Fn.Input :: _ "nSides"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: Texture, nSides :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> geometry <-}
    Family.def
        unit
        { what : ?what_default, nSides : 3 }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            nSides <- P.receive _in_nSides
            -- Kaleid _in_what _in_nSides
            P.send _out_out ?out_out
