module Toolkit.HydraGen.Family.Extsource.FInitCam where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_where = Fn.Input :: _ "where"
_in_index = Fn.Input :: _ "index"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> extsource <-}
    Family.Def Unit
        ( where :: H.Source, index :: H.Value )
        ( out :: H.Unit )
        m

family :: forall m. Family m
family = -- {-> extsource <-}
    Family.def
        unit
        { where : ?where_default, index : ?index_default }
        { out : ?out_default }
        $ Fn.make "initCam" $ do
            where <- P.receive _in_where
            index <- P.receive _in_index
            -- InitCam where index
            P.send _out_out ?out_out
