module Toolkit.HydraGen.Family.Color.FPosterize where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_bins = Fn.Input :: _ "bins"
_in_gamma = Fn.Input :: _ "gamma"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> color <-}
    Family.Def Unit
        ( what :: H.Texture, bins :: H.Value, gamma :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> color <-}
    Family.def
        unit
        { what : H.Empty, bins : H.Number 3.0, gamma : H.Number 0.6 }
        { out : H.Empty }
        $ Fn.make "posterize" $ do
            what <- P.receive _in_what
            bins <- P.receive _in_bins
            gamma <- P.receive _in_gamma
            P.send _out_out $ H.WithColor what $ H.Posterize { bins, gamma }
