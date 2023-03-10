module MPosterize where

import Prelude

_in_what = Fn.Input :: _ "what"
_in_bins = Fn.Input :: _ "bins"
_in_gamma = Fn.Input :: _ "gamma"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> color <-}
    Family.Def Unit
        ( what :: Texture, bins :: Value, gamma :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> color <-}
    Family.def
        unit
        { what : ?what_default, bins : 3, gamma : 0.6 }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            bins <- P.receive _in_bins
            gamma <- P.receive _in_gamma
            -- Posterize _in_what _in_bins _in_gamma
            P.send _out_out ?out_out
