module Toolkit.HydraGen.Family.Source.FPrev where





import Prelude
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family




_out_out = Fn.Output :: _ "out"


type Family m = -- {-> source <-}
    Family.Def Unit
        ( ?ch_type )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> source <-}
    Family.def
        unit
        { ?ch_default }
        { out : ?out_default }
        $ Fn.make $ do
            --
            -- Prev ?input
            P.send _out_out ?out_out
