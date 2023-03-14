module Toolkit.HydraGen.Family.Synth.FSetResolution where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_width = Fn.Input :: _ "width"
_in_height = Fn.Input :: _ "height"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> synth <-}
    Family.Def Unit
        ( width :: H.Value, height :: H.Value )
        ( out :: H.Unit )
        m

family :: forall m. Family m
family = -- {-> synth <-}
    Family.def
        unit
        { width : ?width_default, height : ?height_default }
        { out : ?out_default }
        $ Fn.make "setResolution" $ do
            width <- P.receive _in_width
            height <- P.receive _in_height
            -- SetResolution width height
            P.send _out_out ?out_out
