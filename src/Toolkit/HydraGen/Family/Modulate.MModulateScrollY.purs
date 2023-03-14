module Toolkit.HydraGen.Family.Modulate.FModulateScrollY where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"
_in_scrollY = Fn.Input :: _ "scrollY"
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> modulate <-}
    Family.Def Unit
        ( what :: H.Texture, with :: H.Texture, scrollY :: H.Value, speed :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> modulate <-}
    Family.def
        unit
        { what : ?what_default, with : ?with_default, scrollY : H.0.5, speed : ?speed_default }
        { out : ?out_default }
        $ Fn.make "modulateScrollY" $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            scrollY <- P.receive _in_scrollY
            speed <- P.receive _in_speed
            -- ModulateScrollY what with scrollY speed
            P.send _out_out ?out_out
