module Toolkit.Hydra2.Family.Source.FGradient where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> source <-}
    Family.Def Unit
        ( speed :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> source <-}
    Family.def
        unit
        { speed : H.Number 1.0 }
        { out : H.Empty }
        $ Fn.make "gradient" $ do
            speed <- P.receive _in_speed
            P.send _out_out $ H.From $ H.Gradient { speed }
