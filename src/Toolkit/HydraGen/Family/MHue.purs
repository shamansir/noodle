module MHue where

import Prelude

_in_what = Fn.Input :: _ "what"
_in_hue = Fn.Input :: _ "hue"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> color <-}
    Family.Def Unit
        ( what :: Texture, hue :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> color <-}
    Family.def
        unit
        { what : ?what_default, hue : 0.4 }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            hue <- P.receive _in_hue
            -- Hue _in_what _in_hue
            P.send _out_out ?out_out
