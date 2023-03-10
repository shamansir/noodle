module MInitImage where

import Prelude

_in_where = Fn.Input :: _ "where"
_in_url = Fn.Input :: _ "url"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> extsource <-}
    Family.Def Unit
        ( where :: Source, url :: String )
        ( out :: Unit )
        m

fml :: forall m. Family m
fml = -- {-> extsource <-}
    Family.def
        unit
        { where : ?where_default, url : ?url_default }
        { out : ?out_default }
        $ fml.make $ do
            where <- P.receive _in_where
            url <- P.receive _in_url
            -- InitImage _in_where _in_url
            P.send _out_out ?out_out
