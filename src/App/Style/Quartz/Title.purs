module App.Style.Quartz.Title where

import Color (rgb, rgba) as C

import App.Style (TitleStyle, TitleMode(..))
import Data.Vec2 ((<+>))


title :: TitleStyle
title =
    { mode : InsideBody
    , background : C.rgba 33 33 99 0.5
    , fill : C.rgb 255 255 255
    , size : 20.0
    , padding : 3.0 <+> 10.0
    }