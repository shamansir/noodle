module Toolkit.HydraGen.Family.Color.FPosterize where

import Prelude

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
_in_what = Fn.Input :: _ "what"
_in_bins = Fn.Input :: _ "bins"
_in_gamma = Fn.Input :: _ "gamma"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> color <-}
    Family.Def Unit
        ( what :: Texture, bins :: Value, gamma :: Value )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> color <-}
    Family.def
        unit
        { what : ?what_default, bins : 3, gamma : 0.6 }
        { out : ?out_default }
        $ Fn.make $ do
            what <- P.receive _in_what
            bins <- P.receive _in_bins
            gamma <- P.receive _in_gamma
            -- Posterize what bins gamma
            P.send _out_out ?out_out
