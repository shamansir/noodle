module Blessed.UI.Lists.List.Event where

import Prelude

import Prelude ((<<<))


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed.Core.Key (Key)
import Blessed.Core.Key as Key


import Blessed.Internal.Emitter (class Events, CoreEvent(..)) as C
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



type Handler r = C.Handler r Event


listHandler :: forall r. Event -> Handler r
listHandler = C.handler



on :: forall r. Event -> Handler r
on = listHandler