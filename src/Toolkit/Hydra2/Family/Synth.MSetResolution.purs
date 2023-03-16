module Toolkit.Hydra2.Family.Synth.FSetResolution where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_width = Fn.Input :: _ "width"
_in_height = Fn.Input :: _ "height"


type Family m = -- {-> synth <-}
    Family.Def Unit
        ( width :: H.Value, height :: H.Value )
        ( )
        m


family :: forall m. Family m
family = -- {-> synth <-}
    Family.def
        unit
        { width : H.None, height : H.None }
        { }
        $ Fn.make "setResolution" $ do
            width <- P.receive _in_width
            height <- P.receive _in_height
            -- TODO
            pure unit
