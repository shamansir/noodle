module Toolkit.Hydra2.Family.Color.FBrightness where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family
import Noodle.Node2 (Node) as N


_in_what = Fn.Input :: _ "what"
_in_amount = Fn.Input :: _ "amount"

_out_out = Fn.Output :: _ "out"


type Inputs = ( what :: H.Texture, amount :: H.Value )
type Outputs = ( out :: H.Texture )


defaultInputs :: Record Inputs
defaultInputs = { what : H.Empty, amount : H.Number 0.4 }


defaultOutputs :: Record Outputs
defaultOutputs = { out : H.Empty }


type Family m = -- {-> color <-}
    Family.Def Unit
        Inputs
        Outputs
        m


family :: forall m. Family m
family = -- {-> color <-}
    Family.def
        unit
        defaultInputs
        defaultOutputs
        $ Fn.make "brightness" $ do
            what <- P.receive _in_what
            amount <- P.receive _in_amount
            P.send _out_out $ H.WithColor what $ H.Brightness amount


type Node m =
    N.Node "brightness" Unit
        Inputs
        Outputs
        m