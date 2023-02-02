module Blessed.UI.DataDisplay.ProgressBar.Event where


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))


import Blessed.Internal.BlessedSubj (ProgressBar, class Extends)
import Blessed.Internal.Emitter (class Events, CoreEvent(..), class Fires) as C
import Blessed.Internal.Core (handler, Handler) as C


data Event
    = Init
    | Reset
    | Complete


instance events :: C.Events Event where
    initial = Init

    convert Init = "init" /\ []
    convert Reset = "reset" /\ []
    convert Complete = "complete" /\ []

    toCore _ = C.CoreEvent
    fromCore _ = Nothing


instance C.Fires ProgressBar Event


type Handler subj id r state = C.Handler subj id r state Event


formHandler :: forall subj id r state. Extends ProgressBar subj => C.Fires subj Event => Event -> Handler subj id r state
formHandler = C.handler



on :: forall subj id r state. Extends ProgressBar subj => C.Fires subj Event => Event -> Handler subj id r state
on = formHandler