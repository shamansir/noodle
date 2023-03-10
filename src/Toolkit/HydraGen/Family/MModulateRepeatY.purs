module MModulateRepeatY where

import Prelude

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

fml :: forall m. Family m
fml = -- {-> modulate <-}
    Family.def
        unit
        { what : ?what_default, with : ?with_default, reps : 3, offset : 0.5 }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            reps <- P.receive _in_reps
            offset <- P.receive _in_offset
            -- ModulateRepeatY _in_what _in_with _in_reps _in_offset
            P.send _out_out ?out_out
