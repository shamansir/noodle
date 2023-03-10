module MRepeatY where

import Prelude

_in_what = Fn.Input :: _ "what"
_in_reps = Fn.Input :: _ "reps"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: Texture, reps :: Value, offset :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> geometry <-}
    Family.def
        unit
        { what : ?what_default, reps : 3, offset : ?offset_default }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            reps <- P.receive _in_reps
            offset <- P.receive _in_offset
            -- RepeatY _in_what _in_reps _in_offset
            P.send _out_out ?out_out
