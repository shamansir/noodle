module Blessed.UI.DataDisplay.ProgressBar.Event where


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))


import Blessed.Internal.BlessedSubj (ProgressBar, class Extends)
import Blessed.Internal.Emitter (class Events, class Fires, defaultUniqueId) as C
import Blessed.Internal.Core (handler, Handler) as C


type Event = ProgressBarEvent


data ProgressBarEvent
    = Init
    | Reset
    | Complete


instance events :: C.Events ProgressBarEvent where
    initial = Init

    convert Init = "init" /\ []
    convert Reset = "reset" /\ []
    convert Complete = "complete" /\ []

    uniqueId e = C.defaultUniqueId e


instance C.Fires ProgressBar ProgressBarEvent


formHandler :: forall subj id r state. Extends ProgressBar subj => C.Fires subj ProgressBarEvent => ProgressBarEvent -> C.Handler subj id r state
formHandler = C.handler



on :: forall subj id r state. Extends ProgressBar subj => C.Fires subj ProgressBarEvent => ProgressBarEvent -> C.Handler subj id r state
on = formHandler