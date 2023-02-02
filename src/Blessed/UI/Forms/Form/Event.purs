module Blessed.UI.Forms.Form.Event where


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))


import Blessed.Internal.BlessedSubj (Form, class Extends)
import Blessed.Internal.Emitter (class Events, CoreEvent(..), class Fires) as C
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


instance C.Fires Form Event


type Handler subj id r state = C.Handler subj id r state Event


formHandler :: forall subj id r state. Extends Form subj => C.Fires subj Event => Event -> Handler subj id r state
formHandler = C.handler



on :: forall subj id r state. Extends Form subj => C.Fires subj Event => Event -> Handler subj id r state
on = formHandler