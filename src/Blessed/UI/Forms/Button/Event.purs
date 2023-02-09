module Blessed.UI.Forms.Button.Event where


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))


import Blessed.Internal.BlessedSubj (Button, class Extends)
import Blessed.Internal.Emitter (class Events, class Fires) as C
import Blessed.Internal.Core (handler, Handler) as C


type Event = ButtonEvent


data ButtonEvent
    = Init
    | Press


instance events :: C.Events ButtonEvent where
    initial = Init

    convert Init = "init" /\ []
    convert Press = "press" /\ []


instance C.Fires Button ButtonEvent


type Handler subj id r state = C.Handler subj id r state ButtonEvent


formHandler :: forall subj id r state. Extends Button subj => C.Fires subj ButtonEvent => ButtonEvent -> Handler subj id r state
formHandler = C.handler



on :: forall subj id r state. Extends Button subj => C.Fires subj ButtonEvent => ButtonEvent -> Handler subj id r state
on = formHandler