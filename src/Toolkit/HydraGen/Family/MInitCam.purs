module MInitCam where

import Prelude

_in_where = Fn.Input :: _ "where"
_in_index = Fn.Input :: _ "index"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> extsource <-}
    Family.Def Unit
        ( where :: Source, index :: Value )
        ( out :: Unit )
        m

fml :: forall m. Family m
fml = -- {-> extsource <-}
    Family.def
        unit
        { where : ?where_default, index : ?index_default }
        { out : ?out_default }
        $ fml.make $ do
            where <- P.receive _in_where
            index <- P.receive _in_index
            -- InitCam _in_where _in_index
            P.send _out_out ?out_out
