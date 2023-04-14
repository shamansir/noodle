module Toolkit.Hydra2.Family.Color.FLuma where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s3)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "luma"


name :: String
name = "luma"


type State = Unit


defaultState :: State
defaultState = unit


_in_what      = Fn.Input 1 :: _ "what"
_in_treshold  = Fn.Input 2 :: _ "treshold"
_in_tolerance = Fn.Input 3 :: _ "tolerance"

_out_out      = Fn.Output 1 :: _ "out"


type Inputs = ( what :: H.Texture, treshold :: H.Value, tolerance :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s3 _in_what _in_treshold _in_tolerance


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, treshold : H.Number 0.5, tolerance : H.Number 0.1 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> color <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> color <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            what <- P.receive _in_what
            treshold <- P.receive _in_treshold
            tolerance <- P.receive _in_tolerance
            P.send _out_out $ H.WithColor what $ H.Luma { treshold, tolerance }


type Node (m :: Type -> Type) =
    N.Node "luma" State
        Inputs
        Outputs
        m