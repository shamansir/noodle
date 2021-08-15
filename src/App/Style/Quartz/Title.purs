module App.Style.Quartz.Title where

import Halogen.Svg.Attributes (Color(..))

import App.Style (TitleStyle, TitleMode(..))
import Data.Vec2 ((<+>))


title :: TitleStyle
title =
    { mode : InsideBody
    , background : RGBA 33 33 99 0.5
    , fill : RGB 255 255 255
    , size : 20.0
    , padding : 3.0 <+> 10.0
    }