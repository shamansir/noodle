module Toolkit.HydraGen.Family.Blend.FMult where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"
_in_amount = Fn.Input :: _ "amount"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> blend <-}
    Family.Def Unit
        ( what :: H.Texture, with :: H.Texture, amount :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> blend <-}
    Family.def
        unit
        { what : H.Empty, with : H.Empty, amount : H.Number 1.0 }
        { out : H.Empty }
        $ Fn.make "mult" $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            amount <- P.receive _in_amount
            -- Mult what with amount
            P.send _out_out $ H.BlendOf { what, with } $ H.Mult amount
