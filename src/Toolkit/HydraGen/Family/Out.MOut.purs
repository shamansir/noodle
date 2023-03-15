module Toolkit.HydraGen.Family.Out.FOut where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_target = Fn.Input :: _ "target"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> out <-}
    Family.Def Unit
        ( what :: H.Texture, target :: H.Output )
        ( )
        m

family :: forall m. Family m
family = -- {-> out <-}
    Family.def
        unit
        { what : H.Empty, target : H.Screen }
        { }
        $ Fn.make "out" $ do
            what <- P.receive _in_what
            target <- P.receive _in_target
            pure unit
