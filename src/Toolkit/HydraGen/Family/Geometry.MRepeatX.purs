module Toolkit.HydraGen.Family.Geometry.FRepeatX where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_reps = Fn.Input :: _ "reps"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: H.Texture, reps :: H.Value, offset :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> geometry <-}
    Family.def
        unit
        { what : H.Empty, reps : H.Number 3.0, offset : H.Number 0.0 }
        { out : H.Empty }
        $ Fn.make "repeatX" $ do
            what <- P.receive _in_what
            reps <- P.receive _in_reps
            offset <- P.receive _in_offset
            -- RepeatX what reps offset
            P.send _out_out $ H.Geometry what $ H.GRepeatX { reps, offset }
