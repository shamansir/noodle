module Toolkit.Hydra2.Family.Blend.FDiff where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> blend <-}
    Family.Def Unit
        ( what :: H.Texture, with :: H.Texture )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> blend <-}
    Family.def
        unit
        { what : H.Empty, with : H.Empty }
        { out : H.Empty }
        $ Fn.make "diff" $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            -- Diff what with
            P.send _out_out $ H.BlendOf { what, with } $ H.Diff
