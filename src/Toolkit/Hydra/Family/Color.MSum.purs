module Tookit.Hydra.Family.Color.FSum where


import Tookit.Hydra.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1)
import Data.SOrder (empty) as SOrder
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "sum"


name :: String
name = "sum"


type State = Unit


defaultState :: State
defaultState = unit


_in_what = Fn.Input 0 :: _ "what"


type Inputs = ( what :: H.Texture, todo :: H.TODO )
type Outputs = ( )


inputsOrder :: _
inputsOrder = s1 _in_what


outputsOrder :: _
outputsOrder = SOrder.empty


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, todo : H.TODO }


defaultOutputs :: Record Outputs
defaultOutputs = { }


type Family (m :: Type -> Type) = -- {-> color <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> color <-}
    Family.def
        unit
        { what : H.Empty, todo : H.TODO }
        { }
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            what <- P.receive _in_what
            pure unit


type Node (m :: Type -> Type) =
    N.Node "sum" State
        Inputs
        Outputs
        m