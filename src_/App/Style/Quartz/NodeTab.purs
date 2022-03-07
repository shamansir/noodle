module App.Style.Quartz.NodeTab where

import Color as C

import App.Style (NodeTabStyle)

nodeTab :: NodeTabStyle
nodeTab =
    { background : C.rgb 170 170 170
    , stroke : C.rgba 220 220 220 0.7
    }