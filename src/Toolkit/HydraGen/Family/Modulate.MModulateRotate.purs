module Toolkit.HydraGen.Family.Modulate.FModulateRotate where





import Prelude
import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"
_in_multiple = Fn.Input :: _ "multiple"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> modulate <-}
    Family.Def Unit
        ( what :: Texture, with :: Texture, multiple :: Value, offset :: Value )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> modulate <-}
    Family.def
        unit
        { what : ?what_default, with : ?with_default, multiple : 1, offset : ?offset_default }
        { out : ?out_default }
        $ Fn.make $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            multiple <- P.receive _in_multiple
            offset <- P.receive _in_offset
            -- ModulateRotate what with multiple offset
            P.send _out_out ?out_out
