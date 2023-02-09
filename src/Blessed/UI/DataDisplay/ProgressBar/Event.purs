module Blessed.UI.DataDisplay.ProgressBar.Event where


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))


import Blessed.Internal.BlessedSubj (ProgressBar, class Extends)
import Blessed.Internal.Emitter (class Events, class Fires) as C
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


instance C.Fires ProgressBar ProgressBarEvent


type Handler subj id r state = C.Handler subj id r state ProgressBarEvent


formHandler :: forall subj id r state. Extends ProgressBar subj => C.Fires subj ProgressBarEvent => ProgressBarEvent -> Handler subj id r state
formHandler = C.handler



on :: forall subj id r state. Extends ProgressBar subj => C.Fires subj ProgressBarEvent => ProgressBarEvent -> Handler subj id r state
on = formHandler