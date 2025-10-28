module Noodle.Unsafe.RawProcess where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Effect.Exception as Ex

import Noodle.Id (unsafeInletR, unsafeOutletR) as Id
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Raw.Fn.Process (ProcessM)
import Noodle.Raw.Fn.Process as Raw
import Noodle.Repr.ValueInChannel (class FromValueInChannel, class ToValueInChannel)
import Noodle.Repr.ValueInChannel as ViC


receive :: forall state chrepr m a. MonadThrow Ex.Error m => ToValueInChannel chrepr a => String -> ProcessM state chrepr m a
receive inletName =
    receiveViC inletName
    >>= ViC.resolve
        { accept : pure
        , decline : throwError $ Ex.error "Value declined"
        , empty : throwError $ Ex.error  "Value is empty"
        , missingKey : \keyName -> throwError $ Ex.error $ "Missing key: " <> keyName
        }


send :: forall state chrepr m a. FromValueInChannel a chrepr => String -> a -> ProcessM state chrepr m Unit
send outletName =
    Raw.send (Id.unsafeOutletR outletName) <<< ViC.accept <<< ViC.fromValueInChannel


sendIn :: forall state chrepr m a. FromValueInChannel a chrepr => String -> a -> ProcessM state chrepr m Unit
sendIn inletName =
    Raw.sendIn (Id.unsafeInletR inletName) <<< ViC.accept <<< ViC.fromValueInChannel


receiveViC :: forall state chrepr m a. ToValueInChannel chrepr a => String -> ProcessM state chrepr m (ValueInChannel a)
receiveViC =
    map ViC._backToValue <<< Raw.receive <<< Id.unsafeInletR


sendViC :: forall state chrepr m a. FromValueInChannel a chrepr => String -> ValueInChannel a -> ProcessM state chrepr m Unit
sendViC outletName =
    Raw.send (Id.unsafeOutletR outletName) <<< map ViC.fromValueInChannel


sendInViC ∷ forall state chrepr m a. FromValueInChannel a chrepr => String -> ValueInChannel a -> ProcessM state chrepr m Unit
sendInViC inletName =
    Raw.sendIn (Id.unsafeInletR inletName) <<< map ViC.fromValueInChannel
