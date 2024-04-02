module Toolkit.Hydra.Family.Source.FSrc where


import Toolkit.Hydra.Types as H


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


id = Node.Family :: _ "src"


name :: String
name = "src"


type State = H.CanBeSource


defaultState :: State
defaultState = H.defaultCanBeSource


_out_out = Fn.Output 0 :: _ "out"


type Inputs = ( )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = SOrder.empty


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> source <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> source <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            -- src <- P.receive _in_src
            P.send _out_out $ H.Start $ H.Load H.Output0 -- FIXME


type Node (m :: Type -> Type) =
    N.Node "src" State
        Inputs
        Outputs
        m