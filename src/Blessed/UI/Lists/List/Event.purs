module Blessed.UI.Lists.List.Event where

import Prelude

import Prelude ((<<<))


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed.Core.Key (Key)
import Blessed.Core.Key as Key


import Blessed.Internal.Emitter (class Events, class Fires, defaultUniqueId) as C
import Blessed.Internal.BlessedSubj (List, ListBar, class Extends)
import Blessed.Internal.Core (handler, Handler) as C


type Event = ListEvent


data ListEvent
    = Init
    | Select
    | Cancel
    | Action


instance events :: C.Events ListEvent where
    initial = Init

    convert Init = "init" /\ []
    convert Select = "select" /\ []
    convert Cancel = "cancel" /\ []
    convert Action = "action" /\ []

    uniqueId e = C.defaultUniqueId e


instance C.Fires List ListEvent
instance C.Fires ListBar ListEvent


listHandler
    :: forall subj id r state
     . Extends List subj
    => C.Fires subj ListEvent
    => ListEvent -> C.Handler subj id r state
listHandler = C.handler



on
    :: forall subj id r state
     . Extends List subj
    => C.Fires subj ListEvent
    => ListEvent -> C.Handler subj id r state
on = listHandler