module Toolkit.HydraGen.Family.Modulate.FModulateScrollX where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"
_in_scrollX = Fn.Input :: _ "scrollX"
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> modulate <-}
    Family.Def Unit
        ( what :: H.Texture, with :: H.Texture, scrollX :: H.Value, speed :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> modulate <-}
    Family.def
        unit
        { what : ?what_default, with : ?with_default, scrollX : H.0.5, speed : ?speed_default }
        { out : ?out_default }
        $ Fn.make "modulateScrollX" $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            scrollX <- P.receive _in_scrollX
            speed <- P.receive _in_speed
            -- ModulateScrollX what with scrollX speed
            P.send _out_out ?out_out
