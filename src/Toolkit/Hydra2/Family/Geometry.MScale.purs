module Toolkit.Hydra2.Family.Geometry.FScale where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "scale"


name :: String
name = "scale"


type State = Unit


defaultState :: State
defaultState = unit


_in_what = Fn.Input :: _ "what"
_in_amount = Fn.Input :: _ "amount"
_in_xMult = Fn.Input :: _ "xMult"
_in_yMult = Fn.Input :: _ "yMult"
_in_offsetX = Fn.Input :: _ "offsetX"
_in_offsetY = Fn.Input :: _ "offsetY"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, amount :: H.Value, xMult :: H.Value, yMult :: H.Value, offsetX :: H.Value, offsetY :: H.Value )
type Outputs = ( out :: H.Texture )


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
        $ Fn.make name $ do
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