module Toolkit.Hydra2.Family.Synth.FMouse where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure, discard)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family




_x_out = Fn.Output :: _ "x"
_y_out = Fn.Output :: _ "y"


type Family m = -- {-> synth <-}
    Family.Def Unit
        ( )
        ( x :: H.Value
        , y :: H.Value
        )
        m

family :: forall m. Family m
family = -- {-> synth <-}
    Family.def
        unit
        { }
        { x : H.MouseX, y : H.MouseY }
        $ Fn.make "mouse" $ do
            P.send _x_out H.MouseX
            P.send _y_out H.MouseY
