module Noodle.Unsafe.RawProcess where

import Prelude

import Noodle.Id (unsafeInletR, unsafeOutletR) as Id
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Raw.Fn.Process (ProcessM)
import Noodle.Raw.Fn.Process as Raw
import Noodle.Repr.ValueInChannel (class FromValueInChannel, class ToValueInChannel)
import Noodle.Repr.ValueInChannel as VIC


receive :: forall state chrepr m a. ToValueInChannel chrepr a => String -> ProcessM state chrepr m (ValueInChannel a)
receive =
    map VIC._backToValue <<< Raw.receive <<< Id.unsafeInletR


send :: forall state chrepr m a. FromValueInChannel a chrepr => String -> ValueInChannel a -> ProcessM state chrepr m Unit
send outletName =
    Raw.send (Id.unsafeOutletR outletName) <<< map VIC.fromValueInChannel


sendIn ∷ forall state chrepr m a. FromValueInChannel a chrepr => String -> ValueInChannel a -> ProcessM state chrepr m Unit
sendIn inletName =
    Raw.sendIn (Id.unsafeInletR inletName) <<< map VIC.fromValueInChannel
