module Prev.Web.App.Style.Quartz.Slot where

import Color as C

import Prev.Web.App.Style (SlotStyle, Connector(..), SlotDirection(..), SlotInfoVisibility(..))
import Data.Vec2 ((<+>))


slot :: SlotStyle
slot =
    { connector : Circle 5.0
    , offset : 0.0 <+> 0.0
    , direction : Outside
    , info : Always
    , strokeWidth : 1.5
    , stroke : C.rgb 0 0 0
    , fill : C.rgb 255 255 255
    , dimWhenNoLinks : false
    , label : { color : C.rgb 255 255 255, maxWidth : 20.0 }
    , value : { color : C.rgb 255 255 255, maxWidth : 10.0 }
    }