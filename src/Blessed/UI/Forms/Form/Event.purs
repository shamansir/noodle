module Blessed.UI.Forms.Form.Event where


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))


import Blessed.Internal.Emitter (class Events, CoreEvent(..)) as C
import Blessed.Internal.Core (handler, Handler) as C


data Event
    = Init
    | Submit
    | Cancel
    | Reset


instance events :: C.Events Event where
    initial = Init

    convert Init = "init" /\ []
    convert Submit = "submit" /\ []
    convert Cancel = "cancel" /\ []
    convert Reset = "reset" /\ []

    toCore _ = C.CoreEvent
    fromCore _ = Nothing



type Handler subj id r = C.Handler subj id r Event


formHandler :: forall subj id r. Event -> Handler subj id r
formHandler = C.handler



on :: forall subj id r. Event -> Handler subj id r
on = formHandler