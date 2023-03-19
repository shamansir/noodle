module Toolkit.Hydra2.Family.Geometry.FRepeat where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "repeat"


name :: String
name = "repeat"


_in_what = Fn.Input :: _ "what"
_in_repeatX = Fn.Input :: _ "repeatX"
_in_repeatY = Fn.Input :: _ "repeatY"
_in_offsetX = Fn.Input :: _ "offsetX"
_in_offsetY = Fn.Input :: _ "offsetY"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, repeatX :: H.Value, repeatY :: H.Value, offsetX :: H.Value, offsetY :: H.Value )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, repeatX : H.Number 3.0, repeatY : H.Number 3.0, offsetX : H.Number 0.0, offsetY : H.Number 0.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> geometry <-}
    Family.Def Unit
        Inputs
        Outputs
        m

family :: forall (m :: Type -> Type). Family m
family = -- {-> geometry <-}
    Family.def
        unit
        defaultInputs
        defaultOutputs
        $ Fn.make name $ do
            what <- P.receive _in_what
            repeatX <- P.receive _in_repeatX
            repeatY <- P.receive _in_repeatY
            offsetX <- P.receive _in_offsetX
            offsetY <- P.receive _in_offsetY
            -- Repeat what repeatX repeatY offsetX offsetY
            P.send _out_out $ H.Geometry what $ H.GRepeat { repeatX, repeatY, offsetX, offsetY }


type Node (m :: Type -> Type) =
    N.Node "repeat" Unit
        Inputs
        Outputs
        m