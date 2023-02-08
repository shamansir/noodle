module Blessed.UI.Base.Node.Event where


import Data.Tuple.Nested ((/\))


import Blessed.Internal.Emitter (class Events) as C


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