module Blessed.UI.Base.Element.Event where

import Prelude ((<<<), (<$>), (<>))


import Data.String as String
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed.Core.Key (Key)
import Blessed.Core.Key as Key


import Blessed.Internal.Emitter (class Events, class Fires, defaultUniqueId) as C
import Blessed.Internal.BlessedSubj (Element, Box, List, ListBar, Line, Button, TextBox, class Extends)
import Blessed.Internal.Core (handler, Handler) as C


type Event = ElementEvent


data ElementEvent
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


instance events :: C.Events ElementEvent where
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

    uniqueId (Key []) = "key"
    uniqueId (Key keys) = "key-" <> String.joinWith "-" (Key.toString <$> keys)
    uniqueId e = C.defaultUniqueId e


instance C.Fires Element ElementEvent
instance C.Fires Box ElementEvent
instance C.Fires List ElementEvent
instance C.Fires ListBar ElementEvent
instance C.Fires Line ElementEvent
instance C.Fires Button ElementEvent
instance C.Fires TextBox ElementEvent


elmHandler
    :: forall subj id r state
     . Extends Element subj
    => C.Fires subj ElementEvent
    => ElementEvent -> C.Handler subj id r state
elmHandler = C.handler


key
    :: forall subj id r state
     . Extends Element subj
    => C.Fires subj ElementEvent
    => Array Key -> C.Handler subj id r state
key = elmHandler <<< Key


on
    :: forall subj id r state
     . Extends Element subj
    => C.Fires subj ElementEvent
    => ElementEvent -> C.Handler subj id r state
on = elmHandler


click :: ElementEvent
click = Click