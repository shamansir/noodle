module Toolkit.Hydra2.Family.Modulate.FModulateRepeat where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "modulateRepeat"


name :: String
name = "modulateRepeat"


_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"
_in_repeatX = Fn.Input :: _ "repeatX"
_in_repeatY = Fn.Input :: _ "repeatY"
_in_offsetX = Fn.Input :: _ "offsetX"
_in_offsetY = Fn.Input :: _ "offsetY"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, with :: H.Texture, repeatX :: H.Value, repeatY :: H.Value, offsetX :: H.Value, offsetY :: H.Value )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, with : H.Empty, repeatX : H.Number 3.0, repeatY : H.Number 3.0, offsetX : H.Number 0.5, offsetY : H.Number 0.5 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> modulate <-}
    Family.Def Unit
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> modulate <-}
    Family.def
        unit
        defaultInputs
        defaultOutputs
        $ Fn.make name $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            repeatX <- P.receive _in_repeatX
            repeatY <- P.receive _in_repeatY
            offsetX <- P.receive _in_offsetX
            offsetY <- P.receive _in_offsetY
            P.send _out_out $ H.ModulateWith { what, with } $ H.ModRepeat { repeatX, repeatY, offsetX, offsetY }


type Node (m :: Type -> Type) =
    N.Node "modulateRepeat" Unit
        Inputs
        Outputs
        m