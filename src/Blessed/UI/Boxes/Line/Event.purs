module Blessed.UI.Boxes.Line.Event where


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



type Handler r = C.Handler r Event


lineHandler :: forall r. Event -> Handler r
lineHandler = C.handler



on :: forall r. Event -> Handler r
on = lineHandler