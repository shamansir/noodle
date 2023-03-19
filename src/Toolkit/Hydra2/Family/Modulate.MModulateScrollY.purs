module Toolkit.Hydra2.Family.Modulate.FModulateScrollY where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N


import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "modulateScrollY"


name :: String
name = "modulateScrollY"


type State = Unit


defaultState :: State
defaultState = unit


_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"
_in_scrollY = Fn.Input :: _ "scrollY"
_in_speed = Fn.Input :: _ "speed"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, with :: H.Texture, scrollY :: H.Value, speed :: H.Value )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, with : H.Empty, scrollY : H.Number 0.5, speed : H.Number 1.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> modulate <-}
    Family.Def State
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> modulate <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            scrollY <- P.receive _in_scrollY
            speed <- P.receive _in_speed
            P.send _out_out $ H.ModulateWith { what, with } $ H.ModScrollY { scrollY, speed }


type Node (m :: Type -> Type) =
    N.Node "modulateScrollY" State
        Inputs
        Outputs
        m