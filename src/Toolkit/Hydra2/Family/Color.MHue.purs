module Toolkit.Hydra2.Family.Color.FHue where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "hue"


name :: String
name = "hue"


type State = Unit


defaultState :: State
defaultState = unit


_in_what = Fn.Input :: _ "what"
_in_hue = Fn.Input :: _ "hue"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, hue :: H.Value )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, hue : H.Number 0.4 }


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
        $ Fn.make name $ do
            what <- P.receive _in_what
            hue <- P.receive _in_hue
            P.send _out_out $ H.WithColor what $ H.Hue hue


type Node (m :: Type -> Type) =
    N.Node "hue" State
        Inputs
        Outputs
        m