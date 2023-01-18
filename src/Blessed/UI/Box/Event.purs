module Blessed.UI.Box.Event where

import Prelude ((<<<))


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed.Core.Key (Key)
import Blessed.Core.Key as Key


import Blessed.Internal.Emitter (class Events, CoreEvent(..)) as C
import Blessed.Internal.Core (handler, Handler) as C


data Event
    = Init
    | Key (Array Key)
    | Click
    -- FIXME: Node + Element events


instance events :: C.Events Event where
    initial = Init

    convert Init = "init" /\ []
    convert (Key keys) = "key" /\ Key.convertAll keys
    convert Click = "click" /\ []

    toCore _ = C.CoreEvent
    fromCore _ = Nothing



type BoxHandler r = C.Handler r Event


boxHandler :: forall r. Event -> BoxHandler r
boxHandler = C.handler





key :: forall r. Array Key -> BoxHandler r
key = boxHandler <<< Key


on :: forall r. Event -> BoxHandler r
on = boxHandler


click :: Event
click = Click