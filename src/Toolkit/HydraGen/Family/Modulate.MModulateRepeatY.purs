module Toolkit.HydraGen.Family.Modulate.FModulateRepeatY where





import Prelude
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"
_in_reps = Fn.Input :: _ "reps"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> modulate <-}
    Family.Def Unit
        ( what :: Texture, with :: Texture, reps :: Value, offset :: Value )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> modulate <-}
    Family.def
        unit
        { what : ?what_default, with : ?with_default, reps : 3, offset : 0.5 }
        { out : ?out_default }
        $ Fn.make $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            reps <- P.receive _in_reps
            offset <- P.receive _in_offset
            -- ModulateRepeatY what with reps offset
            P.send _out_out ?out_out
