module Toolkit.HydraGen.Family.Synth.FHush where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_todo = Fn.Input :: _ "todo"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> synth <-}
    Family.Def Unit
        ( todo :: H.TODO )
        ( )
        m


family :: forall m. Family m
family = -- {-> synth <-}
    Family.def
        unit
        { todo : H.TODO }
        { }
        $ Fn.make "hush" $ do
            _ <- P.receive _in_todo
            pure unit
