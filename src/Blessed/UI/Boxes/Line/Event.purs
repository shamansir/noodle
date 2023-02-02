module Blessed.UI.Boxes.Line.Event where


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))


import Blessed.Internal.Core (handler, Handler) as C
import Blessed.Internal.BlessedSubj (Line, class Extends)
import Blessed.Internal.Emitter (class Events, CoreEvent(..), class Fires) as C


data Event
    = Init


instance events :: C.Events Event where
    initial = Init

    convert Init = "init" /\ []

    toCore _ = C.CoreEvent
    fromCore _ = Nothing


instance C.Fires Line Event


type Handler subj id r state = C.Handler subj id r state Event


lineHandler :: forall subj id r state. Extends Line subj => C.Fires subj Event => Event -> Handler subj id r state
lineHandler = C.handler



on :: forall subj id r state. Extends Line subj => C.Fires subj Event => Event -> Handler subj id r state
on = lineHandler