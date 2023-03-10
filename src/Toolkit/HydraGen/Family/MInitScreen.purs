module MInitScreen where

import Prelude

_in_where = Fn.Input :: _ "where"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> extsource <-}
    Family.Def Unit
        ( where :: Source )
        ( out :: Unit )
        m

fml :: forall m. Family m
fml = -- {-> extsource <-}
    Family.def
        unit
        { where : ?where_default }
        { out : ?out_default }
        $ fml.make $ do
            where <- P.receive _in_where
            -- InitScreen _in_where
            P.send _out_out ?out_out
