module Toolkit.HydraGen.Family.Color.FTresh where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_treshold = Fn.Input :: _ "treshold"
_in_tolerance = Fn.Input :: _ "tolerance"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> color <-}
    Family.Def Unit
        ( what :: H.Texture, treshold :: H.Value, tolerance :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> color <-}
    Family.def
        unit
        { what : ?what_default, treshold : H.0.5, tolerance : H.0.1 }
        { out : ?out_default }
        $ Fn.make "tresh" $ do
            what <- P.receive _in_what
            treshold <- P.receive _in_treshold
            tolerance <- P.receive _in_tolerance
            -- Tresh what treshold tolerance
            P.send _out_out ?out_out
