module Source.MSrc where

import Prelude

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family

_in_src = Fn.Input :: _ "src"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> source <-}
    Family.Def Unit
        ( src :: Source )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> source <-}
    Family.def
        unit
        { src : ?src_default }
        { out : ?out_default }
        $ Fn.make $ do
            src <- P.receive _in_src
            -- Src src
            P.send _out_out ?out_out
