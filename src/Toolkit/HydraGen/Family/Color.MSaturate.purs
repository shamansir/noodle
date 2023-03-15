module Toolkit.HydraGen.Family.Color.FSaturate where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_amount = Fn.Input :: _ "amount"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> color <-}
    Family.Def Unit
        ( what :: H.Texture, amount :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> color <-}
    Family.def
        unit
        { what : H.Empty, amount : H.Number 2.0 }
        { out : H.Empty }
        $ Fn.make "saturate" $ do
            what <- P.receive _in_what
            amount <- P.receive _in_amount
            P.send _out_out $ H.WithColor what $ H.Saturate amount
