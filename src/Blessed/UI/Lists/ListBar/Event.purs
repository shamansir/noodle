module Blessed.UI.Lists.ListBar.Event where


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))


import Blessed.Internal.Emitter (class Events, CoreEvent(..)) as C
import Blessed.Internal.Core (handler, Handler) as C


data Event
    = Init


instance events :: C.Events Event where
    initial = Init

    convert Init = "init" /\ []

    toCore _ = C.CoreEvent
    fromCore _ = Nothing



type Handler subj id r = C.Handler subj id r Event


lbHandler :: forall subj id r. Event -> Handler subj id r
lbHandler = C.handler



on :: forall subj id r. Event -> Handler subj id r
on = lbHandler