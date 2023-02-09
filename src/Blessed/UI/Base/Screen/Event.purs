module Blessed.UI.Base.Screen.Event where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed.Core.Key (Key)
import Blessed.Core.Key as Key
import Blessed.Internal.BlessedSubj (Screen, class Extends)
import Blessed.Internal.Emitter (class Events, class Fires) as C
import Blessed.Internal.Core (handler, Handler) as C


type Event = ScreenEvent


data ScreenEvent
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



instance events :: C.Events ScreenEvent where
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


type Handler subj id r state = C.Handler subj id r state ScreenEvent


instance C.Fires Screen ScreenEvent


screenHandler :: forall subj id r state. Extends Screen subj => C.Fires subj ScreenEvent => ScreenEvent -> Handler subj id r state
screenHandler = C.handler


key :: forall subj id r state. Extends Screen subj => C.Fires subj ScreenEvent => Array Key -> Handler subj id r state
key = screenHandler <<< Key
