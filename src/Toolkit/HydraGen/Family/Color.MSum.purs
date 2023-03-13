module Toolkit.HydraGen.Family.Color.FSum where





import Prelude
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"


_out_out = Fn.Output :: _ "out"


type Family m = -- {-> color <-}
    Family.Def Unit
        ( what :: Texture, ?ch_type )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> color <-}
    Family.def
        unit
        { what : ?what_default, ?ch_default }
        { out : ?out_default }
        $ Fn.make $ do
            what <- P.receive _in_what
            --
            -- Sum what ?input
            P.send _out_out ?out_out
