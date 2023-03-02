module Blessed.UI.Forms.Checkbox.Event where


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))


import Blessed.Internal.BlessedSubj (Checkbox, class Extends)
import Blessed.Internal.Emitter (class Events, class Fires, defaultUniqueId) as C
import Blessed.Internal.Core (handler, Handler) as C


type Event = CheckboxEvent


data CheckboxEvent
    = Init
    | Check
    | Uncheck


instance events :: C.Events CheckboxEvent where
    initial = Init

    convert Init = "init" /\ []
    convert Check = "check" /\ []
    convert Uncheck = "uncheck" /\ []

    uniqueId e = C.defaultUniqueId e


instance C.Fires Checkbox CheckboxEvent



cbHandler :: forall subj id r state. Extends Checkbox subj => C.Fires subj CheckboxEvent => CheckboxEvent -> C.Handler subj id r state
cbHandler = C.handler



on :: forall subj id r state. Extends Checkbox subj => C.Fires subj CheckboxEvent => CheckboxEvent -> C.Handler subj id r state
on = cbHandler