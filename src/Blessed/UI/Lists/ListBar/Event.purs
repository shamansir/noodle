module Blessed.UI.Lists.ListBar.Event where


import Data.Tuple.Nested ((/\))


import Blessed.Internal.BlessedSubj (ListBar, class Extends)
import Blessed.Internal.Emitter (class Events, class Fires) as C
import Blessed.Internal.Core (handler, Handler) as C


data Event
    = Init


instance events :: C.Events Event where
    initial = Init

    convert Init = "init" /\ []


instance C.Fires ListBar Event


type Handler subj id r state = C.Handler subj id r state Event


lbHandler :: forall subj id r state. Extends ListBar subj => C.Fires subj Event => Event -> Handler subj id r state
lbHandler = C.handler



on :: forall subj id r state. Extends ListBar subj => C.Fires subj Event => Event -> Handler subj id r state
on = lbHandler