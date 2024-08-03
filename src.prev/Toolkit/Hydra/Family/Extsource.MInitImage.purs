module Toolkit.Hydra.Family.ExternalSources.FInitImage where


import Toolkit.Hydra.Types as H
import Toolkit.Hydra.Repr.Wrap (WrapRepr)


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s2)
import Data.SOrder (empty) as SOrder
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "initImage"


name :: String
name = "initImage"


type State = Unit


defaultState :: State
defaultState = unit


_in_src = Fn.Input 0 :: _ "src"
_in_url = Fn.Input 1 :: _ "url"


type Inputs = ( src :: H.SourceN, url :: H.Url )
type Outputs = ( )


inputsOrder :: _
inputsOrder = s2 _in_src _in_url


outputsOrder :: _
outputsOrder = SOrder.empty


defaultInputs :: Record Inputs
defaultInputs = { src : H.defaultSourceN, url : H.noUrl }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family (m :: Type -> Type) = -- {-> extsource <-}
    Family.Def State
        Inputs
        Outputs
        WrapRepr
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> extsource <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            src <- P.receive _in_src
            url <- P.receive _in_url
            pure unit


type Node (m :: Type -> Type) =
    N.Node "initImage" State
        Inputs
        Outputs
        WrapRepr
        m