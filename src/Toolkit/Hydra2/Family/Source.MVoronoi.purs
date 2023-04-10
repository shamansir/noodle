module Toolkit.Hydra2.Family.Source.FVoronoi where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "voronoi"


name :: String
name = "voronoi"


type State = Unit


defaultState :: State
defaultState = unit


_in_scale = Fn.Input :: _ "scale"
_in_speed = Fn.Input :: _ "speed"
_in_blending = Fn.Input :: _ "blending"

_out_out = Fn.Output :: _ "out"


type Inputs = ( scale :: H.Value, speed :: H.Value, blending :: H.Value )
type Outputs = ( out :: H.Texture )


type InputsOrder :: SOrder
type InputsOrder = "scale" ::: "speed" ::: "blending" ::: T


type OutputsOrder :: SOrder
type OutputsOrder = "out" ::: T


defaultInputs :: Record Inputs
defaultInputs = { scale : H.Number 5.0, speed : H.Number 0.3, blending : H.Number 0.3 }


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
            { inputs : Proxy :: _ InputsOrder, outputs : Proxy :: _ OutputsOrder }
            $ do
            scale <- P.receive _in_scale
            speed <- P.receive _in_speed
            blending <- P.receive _in_blending
            P.send _out_out $ H.From $ H.Voronoi { scale, speed, blending }


type Node (m :: Type -> Type) =
    N.Node "voronoi" State
        Inputs
        Outputs
        m