module Blessed.UI.Node.Event where


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))


import Blessed.Internal.Emitter (class Events, CoreEvent(..)) as C


data Event
    = Init
    | Adopt
    | Remove
    | Reparent
    | Attach
    | Detach


instance events :: C.Events Event where
    initial = Init

    convert Init = "init" /\ []
    convert Adopt = "adopt" /\ []
    convert Remove = "remove" /\ []
    convert Reparent = "reparent" /\ []
    convert Attach = "attach" /\ []
    convert Detach = "detach" /\ []

    toCore _ = C.CoreEvent
    fromCore _ = Nothing