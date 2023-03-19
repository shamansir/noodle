module Toolkit.Hydra2.Family.Blend.FDiff where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N
import Noodle.Id (Family(..)) as Node


id = Node.Family :: _ "diff"


name :: String
name = "diff"


_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, with :: H.Texture )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, with : H.Empty }


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
            -- Diff what with
            P.send _out_out $ H.BlendOf { what, with } $ H.Diff


type Node (m :: Type -> Type) =
    N.Node "diff" Unit
        Inputs
        Outputs
        m