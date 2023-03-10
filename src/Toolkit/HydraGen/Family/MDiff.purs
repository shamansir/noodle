module MDiff where

import Prelude

_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> blend <-}
    Family.Def Unit
        ( what :: Texture, with :: Texture )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> blend <-}
    Family.def
        unit
        { what : ?what_default, with : ?with_default }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            -- Diff _in_what _in_with
            P.send _out_out ?out_out
