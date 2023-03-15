module Toolkit.HydraGen.Family.Source.FNoise where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_scale = Fn.Input :: _ "scale"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> source <-}
    Family.Def Unit
        ( scale :: H.Value, offset :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> source <-}
    Family.def
        unit
        { scale : H.Number 10.0, offset : H.Number 0.1 }
        { out : H.Empty }
        $ Fn.make "noise" $ do
            scale <- P.receive _in_scale
            offset <- P.receive _in_offset
            P.send _out_out $ H.From $ H.Noise { scale, offset }
