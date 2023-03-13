module Toolkit.HydraGen.Family.Source.FOsc where





import Prelude
import Noodle.Fn2 as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2.Process as P
import Noodle.Family.Def as Family


_in_frequency = Fn.Input :: _ "frequency"
_in_sync = Fn.Input :: _ "sync"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"


type Family m = -- {-> source <-}
    Family.Def Unit
        ( frequency :: Value, sync :: Value, offset :: Value )
        ( out :: Texture )
        m

family :: forall m. Family m
family = -- {-> source <-}
    Family.def
        unit
        { frequency : 60, sync : 0.1, offset : ?offset_default }
        { out : ?out_default }
        $ Fn.make $ do
            frequency <- P.receive _in_frequency
            sync <- P.receive _in_sync
            offset <- P.receive _in_offset
            -- Osc frequency sync offset
            P.send _out_out ?out_out
