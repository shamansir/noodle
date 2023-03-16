module Toolkit.Hydra2.Family.Color.FSum where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"


_out_out = Fn.Output :: _ "out"


type Family m = -- {-> color <-}
    Family.Def Unit
        ( what :: H.Texture, todo :: H.TODO )
        ( )
        m

family :: forall m. Family m
family = -- {-> color <-}
    Family.def
        unit
        { what : H.Empty, todo : H.TODO }
        { }
        $ Fn.make "sum" $ do
            what <- P.receive _in_what
            pure unit
