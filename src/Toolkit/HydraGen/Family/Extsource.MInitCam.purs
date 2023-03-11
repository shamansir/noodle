module Extsource.MInitCam where

import Prelude

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family

_in_where = Fn.Input :: _ "where"
_in_index = Fn.Input :: _ "index"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> extsource <-}
    Family.Def Unit
        ( where :: Source, index :: Value )
        ( out :: Unit )
        m

family :: forall m. Family m
family = -- {-> extsource <-}
    Family.def
        unit
        { where : ?where_default, index : ?index_default }
        { out : ?out_default }
        $ Fn.make $ do
            where <- P.receive _in_where
            index <- P.receive _in_index
            -- InitCam where index
            P.send _out_out ?out_out
