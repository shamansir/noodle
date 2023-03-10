module MOffset where

import Prelude

_in_a = Fn.Input :: _ "a"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> array <-}
    Family.Def Unit
        ( a :: Array, offset :: Value )
        ( out :: Value )
        m

fml :: forall m. Family m
fml = -- {-> array <-}
    Family.def
        unit
        { a : ?a_default, offset : 0.5 }
        { out : ?out_default }
        $ fml.make $ do
            a <- P.receive _in_a
            offset <- P.receive _in_offset
            -- Offset _in_a _in_offset
            P.send _out_out ?out_out
