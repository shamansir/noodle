module MSrc where

import Prelude

_in_src = Fn.Input :: _ "src"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> source <-}
    Family.Def Unit
        ( src :: Source )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> source <-}
    Family.def
        unit
        { src : ?src_default }
        { out : ?out_default }
        $ fml.make $ do
            src <- P.receive _in_src
            -- Src _in_src
            P.send _out_out ?out_out
