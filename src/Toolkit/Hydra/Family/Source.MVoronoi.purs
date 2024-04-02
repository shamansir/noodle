module Toolkit.Hydra.Family.Source.FVoronoi where


import Toolkit.Hydra.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node
import Data.SOrder (SOrder, type (:::), T, s1, s3)
import Type.Proxy (Proxy(..))


id = Node.Family :: _ "voronoi"


name :: String
name = "voronoi"


type State = Unit


defaultState :: State
defaultState = unit


_in_scale    = Fn.Input  0 :: _ "scale"
_in_speed    = Fn.Input  1 :: _ "speed"
_in_blending = Fn.Input  2 :: _ "blending"

_out_out     = Fn.Output 0 :: _ "out"


type Inputs = ( scale :: H.Value, speed :: H.Value, blending :: H.Value )
type Outputs = ( out :: H.Texture )


inputsOrder :: _
inputsOrder = s3 _in_scale _in_speed _in_blending


outputsOrder :: _
outputsOrder = s1 _out_out


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
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            scale <- P.receive _in_scale
            speed <- P.receive _in_speed
            blending <- P.receive _in_blending
            P.send _out_out $ H.Start $ H.Voronoi { scale, speed, blending }


type Node (m :: Type -> Type) =
    N.Node "voronoi" State
        Inputs
        Outputs
        m