module Blessed.UI.Base.Element.Event where

import Prelude ((<<<))


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed.Core.Key (Key)
import Blessed.Core.Key as Key


import Blessed.Internal.Emitter (class Events, class Fires) as C
import Blessed.Internal.BlessedSubj (Element, Box, List, ListBar, class Extends)
import Blessed.Internal.Core (handler, Handler) as C


data Event
    = Init
    | Blur
    | Focus
    | WheelDown
    | WheelUp
    | MouseDown
    | MouseUp
    | MouseOver
    | MouseOut
    | MouseMove
    | Click
    | KeyPress
    | Key (Array Key)
    | Move
    | Resize
    | Prerender
    | Render
    | Hide
    | Show
    | Destroy
    -- | Key String


instance events :: C.Events Event where
    initial = Init

    convert Init = "init" /\ []
    convert (Key keys) = "key" /\ Key.convertAll keys
    convert Blur = "blur" /\ []
    convert Focus = "focus" /\ []
    convert WheelDown = "wheeldown" /\ []
    convert WheelUp = "wheelup" /\ []
    convert MouseDown = "mousedown" /\ []
    convert MouseUp = "mouseup" /\ []
    convert MouseOver = "mouseover" /\ []
    convert MouseOut = "mouseout" /\ []
    convert MouseMove = "mousemove" /\ []
    convert Click = "click" /\ []
    convert KeyPress = "keypress" /\ []
    convert Move = "move" /\ []
    convert Resize = "resize" /\ []
    convert Prerender = "prerender" /\ []
    convert Render = "render" /\ []
    convert Hide = "hide" /\ []
    convert Show = "show" /\ []
    convert Destroy = "destroy" /\ []


type Handler subj id r state = C.Handler subj id r state Event


instance C.Fires Element Event
instance C.Fires Box Event
instance C.Fires List Event
instance C.Fires ListBar Event


elmHandler
    :: forall subj id r state
     . Extends Element subj
    => C.Fires subj Event
    => Event -> Handler subj id r state
elmHandler = C.handler


key
    :: forall subj id r state
     . Extends Element subj
    => C.Fires subj Event
    => Array Key -> Handler subj id r state
key = elmHandler <<< Key


on
    :: forall subj id r state
     . Extends Element subj
    => C.Fires subj Event
    => Event -> Handler subj id r state
on = elmHandler


click :: Event
click = Click