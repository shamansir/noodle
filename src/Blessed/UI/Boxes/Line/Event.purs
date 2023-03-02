module Blessed.UI.Boxes.Line.Event where


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))


import Blessed.Internal.Core (handler, Handler) as C
import Blessed.Internal.BlessedSubj (Line, class Extends)
import Blessed.Internal.Emitter (class Events, class Fires, defaultUniqueId) as C


type Event = LineEvent


data LineEvent
    = Init


instance events :: C.Events LineEvent where
    initial = Init

    convert Init = "init" /\ []

    uniqueId e = C.defaultUniqueId e


instance C.Fires Line LineEvent


lineHandler :: forall subj id r state. Extends Line subj => C.Fires subj LineEvent => LineEvent -> C.Handler subj id r state
lineHandler = C.handler



on :: forall subj id r state. Extends Line subj => C.Fires subj LineEvent => LineEvent -> C.Handler subj id r state
on = lineHandler