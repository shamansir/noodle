module Toolkit.Hydra2.Family.Extsource.FInit where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_options = Fn.Input :: _ "options"


type Family m = -- {-> extsource <-}
    Family.Def Unit
        ( options :: H.SourceOptions )
        ( )
        m

family :: forall m. Family m
family = -- {-> extsource <-}
    Family.def
        unit
        { options : { src : H.Canvas } }
        { }
        $ Fn.make "init" $ do
            options <- P.receive _in_options
            pure unit
