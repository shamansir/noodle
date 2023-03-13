module Toolkit.HydraGen.Family.Out.FOut where





import Prelude
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_what = Fn.Input :: _ "what"
_in_where = Fn.Input :: _ "where"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> out <-}
    Family.Def Unit
        ( what :: Texture, where :: Output )
        ( out :: Unit )
        m

family :: forall m. Family m
family = -- {-> out <-}
    Family.def
        unit
        { what : ?what_default, where : ?where_default }
        { out : ?out_default }
        $ Fn.make $ do
            what <- P.receive _in_what
            where <- P.receive _in_where
            -- Out what where
            P.send _out_out ?out_out
