module Blessed.UI.Screen.Event where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed.Core.Key (Key)
import Blessed.Core.Key as Key
import Blessed.Internal.Core (class Events, CoreEvent(..), handler, Handler) as C


data Event
    = Init
    | Key (Array Key)
    | Other


instance events :: C.Events Event where
    initial = Init

    convert Init = "init" /\ []
    convert (Key keys) = "key" /\ Key.convertAll keys
    convert Other = "?" /\ []

    toCore _ = C.CoreEvent
    fromCore _ = Nothing


type ScreenHandler r = C.Handler r Event


screenHandler :: forall r. Event -> ScreenHandler r
screenHandler = C.handler


key :: forall r. Array Key -> ScreenHandler r
key = screenHandler <<< Key
