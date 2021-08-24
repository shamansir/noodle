module App.Style.Hydra.Slot where

import Data.Color (rgb, rgba) as C

import App.Style (SlotStyle, Connector(..), SlotDirection(..), SlotInfoVisibility(..))
import Data.Vec2 ((<+>))


slot :: SlotStyle
slot =
    { connector : Circle 5.0
    , offset : 10.0 <+> 0.0
    , direction : Inside
    , info : Always
    , strokeWidth : 1.5
    , stroke : C.rgb 0 0 0
    , fill : C.rgb 255 255 255
    , label : { color : C.rgb 255 255 255, maxWidth : 30.0 }
    , value : { color : C.rgb 255 255 255, maxWidth : 10.0 }
    }