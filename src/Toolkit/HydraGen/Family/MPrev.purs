module MPrev where

import Prelude



_out_out = Fn.Output :: _ "out"

type Family m = -- {-> source <-}
    Family.Def Unit
        ( ?ch_type )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> source <-}
    Family.def
        unit
        { ?ch_default }
        { out : ?out_default }
        $ fml.make $ do
            --
            -- Prev ?input
            P.send _out_out ?out_out
