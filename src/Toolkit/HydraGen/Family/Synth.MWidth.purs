module Toolkit.HydraGen.Family.Synth.FWidth where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family




_out_out = Fn.Output :: _ "out"


type Family m = -- {-> synth <-}
    Family.Def Unit
        ( )
        ( out :: H.Value )
        m

family :: forall m. Family m
family = -- {-> synth <-}
    Family.def
        unit
        { }
        { out : ?out_default }
        $ Fn.make "width" $ do
            -- Width
            P.send _out_out ?out_out
