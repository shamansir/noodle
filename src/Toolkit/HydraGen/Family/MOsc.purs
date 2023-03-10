module MOsc where

import Prelude

_in_frequency = Fn.Input :: _ "frequency"
_in_sync = Fn.Input :: _ "sync"
_in_offset = Fn.Input :: _ "offset"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> source <-}
    Family.Def Unit
        ( frequency :: Value, sync :: Value, offset :: Value )
        ( out :: Texture )
        m

fml :: forall m. Family m
fml = -- {-> source <-}
    Family.def
        unit
        { frequency : 60, sync : 0.1, offset : ?offset_default }
        { out : ?out_default }
        $ fml.make $ do
            frequency <- P.receive _in_frequency
            sync <- P.receive _in_sync
            offset <- P.receive _in_offset
            -- Osc _in_frequency _in_sync _in_offset
            P.send _out_out ?out_out
