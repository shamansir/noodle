module Blessed.UI.Forms.Form.Event where


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))


import Blessed.Internal.BlessedSubj (Form, class Extends)
import Blessed.Internal.Emitter (class Events, class Fires, defaultUniqueId) as C
import Blessed.Internal.Core (handler, Handler) as C


type Event = FormEvent


data FormEvent
    = Init
    | Submit
    | Cancel
    | Reset


instance events :: C.Events FormEvent where
    initial = Init

    convert Init = "init" /\ []
    convert Submit = "submit" /\ []
    convert Cancel = "cancel" /\ []
    convert Reset = "reset" /\ []

    uniqueId e = C.defaultUniqueId e


instance C.Fires Form FormEvent


formHandler :: forall subj id r state. Extends Form subj => C.Fires subj FormEvent => FormEvent -> C.Handler subj id r state
formHandler = C.handler



on :: forall subj id r state. Extends Form subj => C.Fires subj FormEvent => FormEvent -> C.Handler subj id r state
on = formHandler