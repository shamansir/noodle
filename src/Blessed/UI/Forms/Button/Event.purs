module Blessed.UI.Forms.Button.Event where


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))


import Blessed.Internal.BlessedSubj (Button, class Extends)
import Blessed.Internal.Emitter (class Events, class Fires) as C
import Blessed.Internal.Core (handler, Handler) as C


data Event
    = Init
    | Press


instance events :: C.Events Event where
    initial = Init

    convert Init = "init" /\ []
    convert Press = "press" /\ []


instance C.Fires Button Event


type Handler subj id r state = C.Handler subj id r state Event


formHandler :: forall subj id r state. Extends Button subj => C.Fires subj Event => Event -> Handler subj id r state
formHandler = C.handler



on :: forall subj id r state. Extends Button subj => C.Fires subj Event => Event -> Handler subj id r state
on = formHandler