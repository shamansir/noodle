module App.Style.Quartz.Colors where

import Halogen.Svg.Attributes (Color(..))

import App.Style (Colors)


colors :: Colors
colors =
    { background : RGB 34 34 42
    , nodeTab :
        { background : RGB 170 170 170
        , stroke : RGBA 220 220 220 0.7
        }
    , patchTab :
        { background : RGB 170 170 170
        , stroke : RGBA 0 0 0 0.7
        }
    , slot :
        { stroke : RGB 0 0 0
        , fill : RGB 255 255 255
        , label : RGB 255 255 255
        , value : RGB 255 255 255
        }
    , body :
        { fill : RGB 80 96 126
        , shadow : RGB 0 0 8
        , stroke : RGBA 255 255 255 0.4
        }
    , title :
        { fill:  RGB 255 255 255
        , background: RGBA 33 33 99 0.5
        }
    }