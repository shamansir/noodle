module App.Style.Quartz.Colors where

import Halogen.Svg.Attributes (Color(..))

import App.Style (Colors)


colors :: Colors
colors =
    { background : RGB 34 34 42
    , nodeTabBackground : RGB 170 170 170
    , nodeTabStroke : RGBA 220 220 220 0.7
    , patchTabBackground : RGB 220 220 220
    , patchTabStroke : RGBA 0 0 0 0.7
    , slotStroke : RGB 0 0 0
    , slotFill : RGB 255 255 255
    , slotTextFill : RGB 255 255 255
    , bodyFill : RGB 80 96 126
    , bodyShadow : RGB 0 0 8
    , bodyStroke : RGBA 255 255 255 0.4
    , nodeName :  RGB 255 255 255
    , namePlateBg : RGBA 33 33 99 0.5
    }