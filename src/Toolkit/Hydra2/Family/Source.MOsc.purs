module Toolkit.Hydra2.Family.Source.FOsc where


import Toolkit.Hydra2.Types as H


import Prelude (Unit, unit, ($), bind, pure)
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
        ( frequency :: H.Value, sync :: H.Value, offset :: H.Value )
        ( out :: H.Texture )
        m

family :: forall m. Family m
family = -- {-> source <-}
    Family.def
        unit
        { frequency : H.Number 60.0, sync : H.Number 0.1, offset : H.Number 0.0 }
        { out : H.Empty }
        $ Fn.make "osc" $ do
            frequency <- P.receive _in_frequency
            sync <- P.receive _in_sync
            offset <- P.receive _in_offset
            P.send _out_out $ H.From $ H.Osc { frequency, sync, offset }
