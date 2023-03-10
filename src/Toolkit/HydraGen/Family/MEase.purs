module MEase where

import Prelude

_in_a = Fn.Input :: _ "a"
_in_ease = Fn.Input :: _ "ease"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> array <-}
    Family.Def Unit
        ( a :: Array, ease :: Ease )
        ( out :: Value )
        m

fml :: forall m. Family m
fml = -- {-> array <-}
    Family.def
        unit
        { a : ?a_default, ease : Linear }
        { out : ?out_default }
        $ fml.make $ do
            a <- P.receive _in_a
            ease <- P.receive _in_ease
            -- Ease _in_a _in_ease
            P.send _out_out ?out_out
