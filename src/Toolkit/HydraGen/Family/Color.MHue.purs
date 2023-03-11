module Color.MHue where

import Prelude

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family

_in_what = Fn.Input :: _ "what"
_in_hue = Fn.Input :: _ "hue"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> color <-}
    Family.Def Unit
        ( what :: Texture, hue :: Value )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> color <-}
    Family.def
        unit
        { what : ?what_default, hue : 0.4 }
        { out : ?out_default }
        $ Fn.make $ do
            what <- P.receive _in_what
            hue <- P.receive _in_hue
            -- Hue what hue
            P.send _out_out ?out_out
