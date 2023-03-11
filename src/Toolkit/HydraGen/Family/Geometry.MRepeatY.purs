module Geometry.MRepeatY where

import Prelude

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family

_in_what = Fn.Input :: _ "what"
_in_reps = Fn.Input :: _ "reps"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> geometry <-}
    Family.Def Unit
        ( what :: Texture, reps :: Value, offset :: Value )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> geometry <-}
    Family.def
        unit
        { what : ?what_default, reps : 3, offset : ?offset_default }
        { out : ?out_default }
        $ Fn.make $ do
            what <- P.receive _in_what
            reps <- P.receive _in_reps
            offset <- P.receive _in_offset
            -- RepeatY what reps offset
            P.send _out_out ?out_out
