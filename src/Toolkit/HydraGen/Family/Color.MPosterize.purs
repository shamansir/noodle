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
        { what : ?what_default, bins : H.3, gamma : H.0.6 }
        { out : ?out_default }
        $ Fn.make "posterize" $ do
            what <- P.receive _in_what
            bins <- P.receive _in_bins
            gamma <- P.receive _in_gamma
            -- Posterize what bins gamma
            P.send _out_out ?out_out
