module Toolkit.HydraGen.Family.Modulate.FModulateRepeat where





import Prelude
import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_with = Fn.Input :: _ "with"
_in_repeatX = Fn.Input :: _ "repeatX"
_in_repeatY = Fn.Input :: _ "repeatY"
_in_offsetX = Fn.Input :: _ "offsetX"
_in_offsetY = Fn.Input :: _ "offsetY"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> modulate <-}
    Family.Def Unit
        ( what :: Texture, with :: Texture, repeatX :: Value, repeatY :: Value, offsetX :: Value, offsetY :: Value )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> modulate <-}
    Family.def
        unit
        { what : ?what_default, with : ?with_default, repeatX : 3, repeatY : 3, offsetX : 0.5, offsetY : 0.5 }
        { out : ?out_default }
        $ Fn.make $ do
            what <- P.receive _in_what
            with <- P.receive _in_with
            repeatX <- P.receive _in_repeatX
            repeatY <- P.receive _in_repeatY
            offsetX <- P.receive _in_offsetX
            offsetY <- P.receive _in_offsetY
            -- ModulateRepeat what with repeatX repeatY offsetX offsetY
            P.send _out_out ?out_out
