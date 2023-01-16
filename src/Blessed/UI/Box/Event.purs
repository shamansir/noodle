module Blessed.UI.Box.Event where

import Prelude (identity, Unit, ($), (<<<))


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed.Core.Key (Key)
import Blessed.Core.Key as Key


import Blessed.Internal.Core (class Events, CoreEvent(..), handler, Handler) as C


data Event
    = Init
    | Key (Array Key)
    | Click
    | Other


instance events :: C.Events Event where
    initial = Init

    convert Init = "init" /\ []
    convert (Key keys) = "key" /\ Key.convertAll keys
    convert Click = "click" /\ []
    convert Other = "?" /\ []

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