module Array.MFast where

import Prelude

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family

_in_a = Fn.Input :: _ "a"
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> array <-}
    Family.Def Unit
        ( a :: Array, speed :: Value )
        ( out :: Value )
        m

family :: forall m. Family m
family = -- {-> array <-}
    Family.def
        unit
        { a : ?a_default, speed : 1 }
        { out : ?out_default }
        $ Fn.make $ do
            a <- P.receive _in_a
            speed <- P.receive _in_speed
            -- Fast a speed
            P.send _out_out ?out_out
