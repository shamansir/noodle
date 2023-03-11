module Toolkit.HydraGen.Family.Modulate.FModulateKaleid where

import Prelude

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"
_in_nSides = Fn.Input :: _ "nSides"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> modulate <-}
    Family.Def Unit
        ( what :: Texture, with :: Texture, nSides :: Value )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> modulate <-}
    Family.def
        unit
        { what : ?what_default, with : ?with_default, nSides : 3 }
        { out : ?out_default }
        $ Fn.make $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            nSides <- P.receive _in_nSides
            -- ModulateKaleid what with nSides
            P.send _out_out ?out_out
