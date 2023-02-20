module Blessed.UI.Lists.ListBar.Event where


import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Codec.Argonaut as CA

import Data.Array ((:))
import Data.Tuple.Nested ((/\))

import Blessed.Core.Key (Key)
import Blessed.Core.Key (convertAll) as Key
import Blessed.Internal.BlessedSubj (ListBar, class Extends)
import Blessed.Internal.Emitter (class Events, class Fires) as C
import Blessed.Internal.Core (handler, Handler) as C


type Event = ListBarEvent


data ListBarEvent -- FIXME: same as list?
    = Init
    | Select
    | Command String (Array Key)


instance events :: C.Events ListBarEvent where
    initial = Init

    convert Init = "init" /\ []
    convert Select = "select" /\ []
    convert (Command cmd keys) = "command" /\ encodeJson cmd : Key.convertAll keys


instance C.Fires ListBar ListBarEvent


type Handler subj id r state = C.Handler subj id r state ListBarEvent


lbHandler :: forall subj id r state. Extends ListBar subj => C.Fires subj ListBarEvent => ListBarEvent -> Handler subj id r state
lbHandler = C.handler



on :: forall subj id r state. Extends ListBar subj => C.Fires subj ListBarEvent => ListBarEvent -> Handler subj id r state
on = lbHandler