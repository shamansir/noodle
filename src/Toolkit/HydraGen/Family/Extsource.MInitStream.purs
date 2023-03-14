module Toolkit.HydraGen.Family.Extsource.FInitStream where


import Toolkit.HydraGen.Types as H


import Prelude (Unit, unit, ($), bind, pure)
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_where = Fn.Input :: _ "where"


_out_out = Fn.Output :: _ "out"


type Family m = -- {-> extsource <-}
    Family.Def Unit
        ( where :: H.Source, ?ch_type )
        ( out :: H.Unit )
        m

family :: forall m. Family m
family = -- {-> extsource <-}
    Family.def
        unit
        { where : ?where_default, ?ch_default }
        { out : ?out_default }
        $ Fn.make "initStream" $ do
            where <- P.receive _in_where
            --
            -- InitStream where ?input
            P.send _out_out ?out_out
