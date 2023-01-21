module Blessed.UI.Box.Event where

import Blessed.Core.Key (Key)


import Blessed.Internal.Core (handler) as C
import Blessed.UI.Element.Event as E


type Event = E.Event


type Handler r = E.Handler r


boxHandler :: forall r. Event -> Handler r
boxHandler = C.handler




key :: forall r. Array Key -> Handler r
key = E.key


on :: forall r. Event -> Handler r
on = E.on


click :: Event
click = E.click