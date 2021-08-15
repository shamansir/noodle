module App.Style.Quartz.NodeTab where

import Halogen.Svg.Attributes (Color(..))

import App.Style (NodeTabStyle)

nodeTab :: NodeTabStyle
nodeTab =
    { background : RGB 170 170 170
    , stroke : RGBA 220 220 220 0.7
    }