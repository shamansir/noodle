module MGradient where

import Prelude

_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> source <-}
    Family.Def Unit
        ( speed :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> source <-}
    Family.def
        unit
        { speed : ?speed_default }
        { out : ?out_default }
        $ fml.make $ do
            speed <- P.receive _in_speed
            -- Gradient _in_speed
            P.send _out_out ?out_out
