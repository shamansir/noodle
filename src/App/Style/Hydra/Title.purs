module App.Style.Hydra.Title where

import Halogen.Svg.Attributes (Color(..))

import App.Style (TitleStyle, TitleMode(..))
import Data.Vec2 ((<+>))


title :: TitleStyle
title =
    { mode : OutsideBody
    , background : RGBA 0 0 0 0.0
    , fill : RGB 255 255 255
    , size : 20.0
    , padding : 3.0 <+> 10.0
    }