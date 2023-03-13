module Toolkit.HydraGen.Family.Extsource.FInitImage where





import Prelude
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_where = Fn.Input :: _ "where"
_in_url = Fn.Input :: _ "url"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> extsource <-}
    Family.Def Unit
        ( where :: Source, url :: String )
        ( out :: Unit )
        m

family :: forall m. Family m
family = -- {-> extsource <-}
    Family.def
        unit
        { where : ?where_default, url : ?url_default }
        { out : ?out_default }
        $ Fn.make $ do
            where <- P.receive _in_where
            url <- P.receive _in_url
            -- InitImage where url
            P.send _out_out ?out_out
