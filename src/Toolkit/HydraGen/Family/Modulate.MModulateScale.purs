module Toolkit.HydraGen.Family.Modulate.FModulateScale where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"
_in_multiple = Fn.Input :: _ "multiple"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> modulate <-}
    Family.Def Unit
        ( what :: H.Texture, with :: H.Texture, multiple :: H.Value, offset :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> modulate <-}
    Family.def
        unit
        { what : H.Empty, with : H.Empty, multiple : H.Number 1.0, offset : H.Number 0.0 }
        { out : H.Empty }
        $ Fn.make "modulateScale" $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            multiple <- P.receive _in_multiple
            offset <- P.receive _in_offset
            P.send _out_out $ H.ModulateWith { what, with } $ H.ModScale { multiple, offset }
