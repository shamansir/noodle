module Toolkit.HydraGen.Family.Array.FFast where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_a = Fn.Input :: _ "a"
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> array <-}
    Family.Def Unit
        ( a :: H.Array, speed :: H.Value )
        ( out :: H.Value )
        m

family :: forall m. Family m
family = -- {-> array <-}
    Family.def
        unit
        { a : ?a_default, speed : H.1 }
        { out : ?out_default }
        $ Fn.make "fast" $ do
            a <- P.receive _in_a
            speed <- P.receive _in_speed
            -- Fast a speed
            P.send _out_out ?out_out
