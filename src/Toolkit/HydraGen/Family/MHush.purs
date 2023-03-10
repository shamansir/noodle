module MHush where

import Prelude

_in_Unit = Fn.Input :: _ "Unit"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> synth <-}
    Family.Def Unit
        ( Unit :: Unknown )
        ( out :: Unit )
        m

fml :: forall m. Family m
fml = -- {-> synth <-}
    Family.def
        unit
        { Unit : ?Unit_default }
        { out : ?out_default }
        $ fml.make $ do
            Unit <- P.receive _in_Unit
            -- Hush _in_Unit
            P.send _out_out ?out_out
