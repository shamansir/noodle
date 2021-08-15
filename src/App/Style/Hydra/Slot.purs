module App.Style.Hydra.Slot where

import Halogen.Svg.Attributes (Color(..))

import App.Style (SlotStyle, Connector(..), SlotDirection(..), SlotInfoVisibility(..))
import Data.Vec2 ((<+>))


slot :: SlotStyle
slot =
    { connector : Circle 5.0
    , offset : 8.0 <+> 0.0
    , direction : Inside
    , info : Always
    , strokeWidth : 1.5
    , stroke : RGB 0 0 0
    , fill : RGB 255 255 255
    , label : { color : RGB 255 255 255, maxWidth : 20.0 }
    , value : { color : RGB 255 255 255, maxWidth : 10.0 }
    }