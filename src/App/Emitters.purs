module App.Emitters where


import Prelude ((<<<), map)

import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import Web.HTML.Window (document)
import Web.Event.Event (EventType)
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes as MET
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLDocument (HTMLDocument)



mouse :: forall t. EventType -> HTMLDocument -> (ME.MouseEvent -> t) -> HS.Emitter t
mouse evtType document handler =
    eventListener
        evtType
        (HTMLDocument.toEventTarget document)
        (map handler <<< ME.fromEvent)


mouseDown :: forall t. HTMLDocument -> (ME.MouseEvent -> t) -> HS.Emitter t
mouseDown = mouse MET.mousedown


mouseUp :: forall t. HTMLDocument -> (ME.MouseEvent -> t) -> HS.Emitter t
mouseUp = mouse MET.mouseup


mouseMove :: forall t. HTMLDocument -> (ME.MouseEvent -> t) -> HS.Emitter t
mouseMove = mouse MET.mousemove


-- TODO: animationFrame