module Toolkit.HydraGen.Family.Array.FEase where

import Prelude

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
_in_a = Fn.Input :: _ "a"
_in_ease = Fn.Input :: _ "ease"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> array <-}
    Family.Def Unit
        ( a :: Array, ease :: Ease )
        ( out :: Value )
        m

family :: forall m. Family m
family = -- {-> array <-}
    Family.def
        unit
        { a : ?a_default, ease : Linear }
        { out : ?out_default }
        $ Fn.make $ do
            a <- P.receive _in_a
            ease <- P.receive _in_ease
            -- Ease a ease
            P.send _out_out ?out_out
