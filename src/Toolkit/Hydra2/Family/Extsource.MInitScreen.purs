module Toolkit.Hydra2.Family.Extsource.FInitScreen where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_src = Fn.Input :: _ "src"

type Family m = -- {-> extsource <-}
    Family.Def Unit
        ( src :: H.Source )
        ( )
        m

family :: forall m. Family m
family = -- {-> extsource <-}
    Family.def
        unit
        { src : H.defaultSource }
        { }
        $ Fn.make "initScreen" $ do
            src <- P.receive _in_src
            pure unit
