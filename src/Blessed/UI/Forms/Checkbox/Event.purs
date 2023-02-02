module Blessed.UI.Forms.Checkbox.Event where


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))


import Blessed.Internal.BlessedSubj (Checkbox, class Extends)
import Blessed.Internal.Emitter (class Events, CoreEvent(..), class Fires) as C
import Blessed.Internal.Core (handler, Handler) as C


data Event
    = Init
    | Check
    | Uncheck


instance events :: C.Events Event where
    initial = Init

    convert Init = "init" /\ []
    convert Check = "check" /\ []
    convert Uncheck = "uncheck" /\ []

    toCore _ = C.CoreEvent
    fromCore _ = Nothing


instance C.Fires Checkbox Event



type Handler subj id r state = C.Handler subj id r state Event


cbHandler :: forall subj id r state. Extends Checkbox subj => C.Fires subj Event => Event -> Handler subj id r state
cbHandler = C.handler



on :: forall subj id r state. Extends Checkbox subj => C.Fires subj Event => Event -> Handler subj id r state
on = cbHandler