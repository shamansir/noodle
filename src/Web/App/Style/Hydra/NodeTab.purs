module Web.App.Style.Hydra.NodeTab where

import Color (rgb, rgba) as C

import Web.App.Style (NodeTabStyle)

nodeTab :: NodeTabStyle
nodeTab =
    { background : C.rgb 170 170 170
    , stroke : C.rgba 220 220 220 0.7
    }