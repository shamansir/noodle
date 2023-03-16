module Toolkit.Hydra2.Family.Source.FSrc where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_src = Fn.Input :: _ "src"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> source <-}
    Family.Def Unit
        ( src :: H.From )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> source <-}
    Family.def
        unit
        { src : H.All }
        { out : H.Empty }
        $ Fn.make "src" $ do
            src <- P.receive _in_src
            P.send _out_out $ H.From $ H.Source src
