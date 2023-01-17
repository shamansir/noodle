module Blessed.UI.Screen.Event where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed.Core.Key (Key)
import Blessed.Core.Key as Key
import Blessed.Internal.Core (class Events, CoreEvent(..), handler, Handler) as C


-- [|=]\s+(\w+)(.*)?$
-- convert $1 = "\L$1\E" /\\ [$2]\n

data Event
    = Init
    | Key (Array Key)
    | Resize
    | Mouse
    | Keypress
    | ElementN String -- NodeId
    | KeyN String-- NodeId
    | Focus
    | Blur
    | Prerender
    | Render
    | Warning
    | Destroy
    | Other



instance events :: C.Events Event where
    initial = Init

    convert Init = "init" /\ []
    convert (Key keys) = "key" /\ Key.convertAll keys
    convert Resize = "resize" /\ []
    convert Mouse = "mouse" /\ []
    convert Keypress = "keypress" /\ []
    convert (ElementN nodeId) = ("element" <> " " <> nodeId) /\ []
    convert (KeyN nodeId) = ("key" <> " " <> nodeId) /\ []
    convert Focus = "focus" /\ []
    convert Blur = "blur" /\ []
    convert Prerender = "prerender" /\ []
    convert Render = "render" /\ []
    convert Warning = "warning" /\ []
    convert Destroy = "destroy" /\ []
    convert Other = "other" /\ []

    toCore _ = C.CoreEvent
    fromCore _ = Nothing


type ScreenHandler r = C.Handler r Event


screenHandler :: forall r. Event -> ScreenHandler r
screenHandler = C.handler


key :: forall r. Array Key -> ScreenHandler r
key = screenHandler <<< Key
