module Toolkit.Hydra2.Family.Extsource.FInitImage where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_src = Fn.Input :: _ "src"
_in_url = Fn.Input :: _ "url"


type Family m = -- {-> extsource <-}
    Family.Def Unit
        ( src :: H.Source, url :: String )
        ( )
        m

family :: forall m. Family m
family = -- {-> extsource <-}
    Family.def
        unit
        { src : H.defaultSource, url : "" }
        { }
        $ Fn.make "initImage" $ do
            src <- P.receive _in_src
            url <- P.receive _in_url
            pure unit
