module Blessed.UI.Screen.Event where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed.Core.Key (Key)
import Blessed.Core.Key as Key
import Blessed.Internal.Emitter (class Events, CoreEvent(..)) as C
import Blessed.Internal.Core (handler, Handler) as C


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

    toCore _ = C.CoreEvent
    fromCore _ = Nothing


type Handler r = C.Handler r Event


screenHandler :: forall r. Event -> Handler r
screenHandler = C.handler


key :: forall r. Array Key -> Handler r
key = screenHandler <<< Key
