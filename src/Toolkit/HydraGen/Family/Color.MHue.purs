module Toolkit.HydraGen.Family.Color.FHue where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_hue = Fn.Input :: _ "hue"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> color <-}
    Family.Def Unit
        ( what :: H.Texture, hue :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> color <-}
    Family.def
        unit
        { what : ?what_default, hue : H.0.4 }
        { out : ?out_default }
        $ Fn.make "hue" $ do
            what <- P.receive _in_what
            hue <- P.receive _in_hue
            -- Hue what hue
            P.send _out_out ?out_out
