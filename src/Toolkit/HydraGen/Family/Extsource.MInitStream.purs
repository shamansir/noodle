module Toolkit.HydraGen.Family.Extsource.FInitStream where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_src = Fn.Input :: _ "src"


type Family m = -- {-> extsource <-}
    Family.Def Unit
        ( src :: H.Source, todo :: H.TODO )
        ( )
        m

family :: forall m. Family m
family = -- {-> extsource <-}
    Family.def
        unit
        { src : H.Source0, todo : H.TODO }
        { }
        $ Fn.make "initStream" $ do
            src <- P.receive _in_src
            pure unit
