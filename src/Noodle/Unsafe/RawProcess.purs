module Noodle.Unsafe.RawProcess where

import Prelude

import Noodle.Id (unsafeInletR, unsafeOutletR) as Id
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Raw.Fn.Process (ProcessM)
import Noodle.Raw.Fn.Process as Raw


receive :: forall state chrepr m. String -> ProcessM state chrepr m (ValueInChannel chrepr)
receive =
    Raw.receive <<< Id.unsafeInletR


send :: forall state chrepr m. String -> ValueInChannel chrepr -> ProcessM state chrepr m Unit
send =
    Raw.send <<< Id.unsafeOutletR


sendIn ∷ forall state chrepr m. String -> ValueInChannel chrepr -> ProcessM state chrepr m Unit
sendIn =
    Raw.sendIn <<< Id.unsafeInletR
