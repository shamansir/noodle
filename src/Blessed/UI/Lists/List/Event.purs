module Blessed.UI.Lists.List.Event where

import Prelude

import Prelude ((<<<))


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed.Core.Key (Key)
import Blessed.Core.Key as Key


import Blessed.Internal.Emitter (class Events, CoreEvent(..), class Fires) as C
import Blessed.Internal.BlessedSubj (Element, Box, List, ListBar, class Extends)
import Blessed.Internal.Core (handler, Handler) as C


data Event
    = Init
    | Select
    | Cancel
    | Action


instance events :: C.Events Event where
    initial = Init

    convert Init = "init" /\ []
    convert Select = "select" /\ []
    convert Cancel = "cancel" /\ []
    convert Action = "action" /\ []

    toCore _ = C.CoreEvent
    fromCore _ = Nothing


instance C.Fires List Event
instance C.Fires ListBar Event


type Handler subj id r state = C.Handler subj id r state Event


listHandler
    :: forall subj id r state
     . Extends List subj
    => C.Fires subj Event
    => Event -> Handler subj id r state
listHandler = C.handler



on
    :: forall subj id r state
     . Extends List subj
    => C.Fires subj Event
    => Event -> Handler subj id r state
on = listHandler