module MFast where

import Prelude

_in_a = Fn.Input :: _ "a"
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> array <-}
    Family.Def Unit
        ( a :: Array, speed :: Value )
        ( out :: Value )
        m

fml :: forall m. Family m
fml = -- {-> array <-}
    Family.def
        unit
        { a : ?a_default, speed : 1 }
        { out : ?out_default }
        $ fml.make $ do
            a <- P.receive _in_a
            speed <- P.receive _in_speed
            -- Fast _in_a _in_speed
            P.send _out_out ?out_out
