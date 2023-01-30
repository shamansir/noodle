module Blessed.UI.Boxes.Box.Event where

import Blessed.Core.Key (Key)


import Blessed.Internal.Core (handler) as C
import Blessed.UI.Base.Element.Event as E


type Event = E.Event


type Handler subj id r = E.Handler subj id r


boxHandler :: forall subj id r. Event -> Handler subj id r
boxHandler = C.handler




key :: forall subj id r. Array Key -> Handler subj id r
key = E.key


on :: forall subj id r. Event -> Handler subj id r
on = E.on


click :: Event
click = E.click