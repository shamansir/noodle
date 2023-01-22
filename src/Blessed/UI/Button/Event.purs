module Blessed.UI.Button.Event where


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))


import Blessed.Internal.Emitter (class Events, CoreEvent(..)) as C
import Blessed.Internal.Core (handler, Handler) as C


data Event
    = Init
    | Press


instance events :: C.Events Event where
    initial = Init

    convert Init = "init" /\ []
    convert Press = "press" /\ []

    toCore _ = C.CoreEvent
    fromCore _ = Nothing



type Handler r = C.Handler r Event


formHandler :: forall r. Event -> Handler r
formHandler = C.handler



on :: forall r. Event -> Handler r
on = formHandler