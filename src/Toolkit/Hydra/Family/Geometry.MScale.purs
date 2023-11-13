module Tookit.Hydra.Family.Geometry.FScale where


import Tookit.Hydra.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s6)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "scale"


name :: String
name = "scale"


type State = Unit


defaultState :: State
defaultState = unit


_in_what    = Fn.Input  0 :: _ "what"
_in_amount  = Fn.Input  1 :: _ "amount"
_in_xMult   = Fn.Input  2 :: _ "xMult"
_in_yMult   = Fn.Input  3 :: _ "yMult"
_in_offsetX = Fn.Input  4 :: _ "offsetX"
_in_offsetY = Fn.Input  5 :: _ "offsetY"

_out_out    = Fn.Output 0 :: _ "out"


type Inputs = ( what :: H.Texture, amount :: H.Value, xMult :: H.Value, yMult :: H.Value, offsetX :: H.Value, offsetY :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s6 _in_what _in_amount _in_xMult _in_yMult _in_offsetX _in_offsetY


outputsOrder :: _
outputsOrder = s1 _out_out


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, amount : H.Number 1.5, xMult : H.Number 1.0, yMult : H.Number 1.0, offsetX : H.Number 0.5, offsetY : H.Number 0.5 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> geometry <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> geometry <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            what <- P.receive _in_what
            amount <- P.receive _in_amount
            xMult <- P.receive _in_xMult
            yMult <- P.receive _in_yMult
            offsetX <- P.receive _in_offsetX
            offsetY <- P.receive _in_offsetY
            P.send _out_out $ H.Geometry what $ H.GScale { amount, xMult, yMult, offsetX, offsetY }


type Node (m :: Type -> Type) =
    N.Node "scale" State
        Inputs
        Outputs
        m