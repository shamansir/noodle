module Blessed.UI.Boxes.Box.Event where

import Blessed.Core.Key (Key)
import Blessed.Internal.BlessedSubj (Element, Box, class Extends)
import Blessed.Internal.Core (handler) as C
import Blessed.Internal.Emitter (class Fires) as C
import Blessed.UI.Base.Element.Event as E


type Event = BoxEvent


type BoxEvent = E.ElementEvent


type Handler subj id r state = E.Handler subj id r state


boxHandler :: forall subj id r state. Extends Element subj => Extends Box subj => C.Fires subj BoxEvent => BoxEvent -> Handler subj id r state
boxHandler = C.handler


key :: forall subj id r state. Extends Element subj => Extends Box subj => C.Fires subj BoxEvent => Array Key -> Handler subj id r state
key = E.key


on :: forall subj id r state. Extends Element subj => Extends Box subj => C.Fires subj BoxEvent => BoxEvent -> Handler subj id r state
on = E.on


click :: BoxEvent
click = E.click