module Toolkit.Hydra2.Family.Extsource.FInitImage where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N


_in_src = Fn.Input :: _ "src"
_in_url = Fn.Input :: _ "url"


type Inputs = ( src :: H.Source, url :: String )
type Outputs = ( )


defaultInputs :: Record Inputs
defaultInputs = { src : H.defaultSource, url : "" }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family m = -- {-> extsource <-}
    Family.Def Unit
        Inputs
        Outputs
        m


family :: forall m. Family m
family = -- {-> extsource <-}
    Family.def
        unit
        defaultInputs
        defaultOutputs
        $ Fn.make "initImage" $ do
            src <- P.receive _in_src
            url <- P.receive _in_url
            pure unit


type Node m =
    N.Node "initImage" Unit
        Inputs
        Outputs
        m