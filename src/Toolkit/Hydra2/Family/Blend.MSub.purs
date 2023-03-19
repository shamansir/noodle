module Toolkit.Hydra2.Family.Blend.FSub where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "sub"


name :: String
name = "sub"


_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"
_in_amount = Fn.Input :: _ "amount"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, with :: H.Texture, amount :: H.Value )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, with : H.Empty, amount : H.Number 1.0 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family (m :: Type -> Type) = -- {-> blend <-}
    Family.Def Unit
        Inputs
        Outputs
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> blend <-}
    Family.def
        unit
        defaultInputs
        defaultOutputs
        $ Fn.make name $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            amount <- P.receive _in_amount
            -- Sub what with amount
            P.send _out_out $ H.BlendOf { what, with } $ H.Sub amount


type Node (m :: Type -> Type) =
    N.Node "sub" Unit
        Inputs
        Outputs
        m