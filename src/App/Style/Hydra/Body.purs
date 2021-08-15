module App.Style.Hydra.Body where

import Halogen.Svg.Attributes (Color(..))

import App.Style (BodyStyle, ShadowType(..))
import Data.Vec2 ((<+>))


body :: BodyStyle
body =
    { shadow : Solid { offset : 5.0 <+> 5.0 }
    , size : 100.0
    , margin : 50.0 <+> 10.0
    , fill : RGBA 0 0 0 0.9
    , stroke : RGB 0 0 0
    , strokeWidth : 1.0
    , cornerRadius : 5.0
    }