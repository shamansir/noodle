module Toolkit.Hydra2.Family.Color.FLuma where


import Toolkit.Hydra2.Types as H


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
        { what : H.Empty, treshold : H.Number 0.5, tolerance : H.Number 0.1 }
        { out : H.Empty }
        $ Fn.make "luma" $ do
            what <- P.receive _in_what
            treshold <- P.receive _in_treshold
            tolerance <- P.receive _in_tolerance
            P.send _out_out $ H.WithColor what $ H.Luma { treshold, tolerance }
