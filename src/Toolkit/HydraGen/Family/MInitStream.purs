module MInitStream where

import Prelude

_in_where = Fn.Input :: _ "where"


_out_out = Fn.Output :: _ "out"

type Family m = -- {-> extsource <-}
    Family.Def Unit
        ( where :: Source, ?ch_type )
        ( out :: Unit )
        m

fml :: forall m. Family m
fml = -- {-> extsource <-}
    Family.def
        unit
        { where : ?where_default, ?ch_default }
        { out : ?out_default }
        $ fml.make $ do
            where <- P.receive _in_where
            --
            -- InitStream _in_where ?input
            P.send _out_out ?out_out
