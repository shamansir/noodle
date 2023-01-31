module Blessed.UI.Forms.Checkbox.Event where


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))


import Blessed.Internal.Emitter (class Events, CoreEvent(..)) as C
import Blessed.Internal.Core (handler, Handler) as C


data Event
    = Init
    | Check
    | Uncheck


instance events :: C.Events Event where
    initial = Init

    convert Init = "init" /\ []
    convert Check = "check" /\ []
    convert Uncheck = "uncheck" /\ []

    toCore _ = C.CoreEvent
    fromCore _ = Nothing



type Handler subj id r = C.Handler subj id r Event


cbHandler :: forall subj id r. Event -> Handler subj id r
cbHandler = C.handler



on :: forall subj id r. Event -> Handler subj id r
on = cbHandler