module MOut where

import Prelude

_in_what = Fn.Input :: _ "what"
_in_where = Fn.Input :: _ "where"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> out <-}
    Family.Def Unit
        ( what :: Texture, where :: Output )
        ( out :: Unit )
        m

fml :: forall m. Family m
fml = -- {-> out <-}
    Family.def
        unit
        { what : ?what_default, where : ?where_default }
        { out : ?out_default }
        $ fml.make $ do
            what <- P.receive _in_what
            where <- P.receive _in_where
            -- Out _in_what _in_where
            P.send _out_out ?out_out
