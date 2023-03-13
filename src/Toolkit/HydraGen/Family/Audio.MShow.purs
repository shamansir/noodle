module Toolkit.HydraGen.Family.Audio.FShow where





import Prelude
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_a = Fn.Input :: _ "a"


_out_out = Fn.Output :: _ "out"


type Family m = -- {-> audio <-}
    Family.Def Unit
        ( a :: Audio, ?ch_type )
        ( out :: Unit )
        m

family :: forall m. Family m
family = -- {-> audio <-}
    Family.def
        unit
        { a : ?a_default, ?ch_default }
        { out : ?out_default }
        $ Fn.make $ do
            a <- P.receive _in_a
            --
            -- Show a ?input
            P.send _out_out ?out_out
