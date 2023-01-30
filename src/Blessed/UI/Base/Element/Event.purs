module Blessed.UI.Base.Element.Event where

import Prelude ((<<<))


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed.Core.Key (Key)
import Blessed.Core.Key as Key


import Blessed.Internal.Emitter (class Events, CoreEvent(..)) as C
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

    toCore _ = C.CoreEvent
    fromCore _ = Nothing



type Handler subj id r = C.Handler subj id r Event


elmHandler :: forall subj id r. Event -> Handler subj id r
elmHandler = C.handler


key :: forall subj id r. Array Key -> Handler subj id r
key = elmHandler <<< Key


on :: forall subj id r. Event -> Handler subj id r
on = elmHandler


click :: Event
click = Click