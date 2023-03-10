module MSum where

import Prelude

_in_what = Fn.Input :: _ "what"


_out_out = Fn.Output :: _ "out"

type Family m = -- {-> color <-}
    Family.Def Unit
        ( what :: Texture, ?ch_type )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> color <-}
    Family.def
        unit
        { what : ?what_default, ?ch_default }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            --
            -- Sum _in_what ?input
            P.send _out_out ?out_out
