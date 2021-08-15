module App.Style.Quartz.Body where

import Halogen.Svg.Attributes (Color(..))

import App.Style (BodyStyle, ShadowType(..))
import Data.Vec2 ((<+>))


body :: BodyStyle
body =
    { shadow : Solid { offset : 5.0 <+> 5.0 }
    , size : 100.0
    , margin : 50.0 <+> 10.0
    , fill : RGB 80 96 126
    , stroke : RGBA 255 255 255 0.4
    , strokeWidth : 1.0
    , cornerRadius : 0.0
    }