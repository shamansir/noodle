module Web.Paths where

import Prelude

import Halogen.Svg.Attributes as HSA


type RectParams =
    { slope :: Number
    , width :: Number
    , height :: Number
    }


nodeTitle :: RectParams -> Array HSA.PathCommand
nodeTitle p =
    [ HSA.m HSA.Abs p.width 0.0
    , HSA.l HSA.Abs 0.0 p.slope
    , HSA.l HSA.Abs 0.0 $ p.height - p.slope
    , HSA.l HSA.Abs p.width p.height
    , HSA.z
    ]


nodeBodyBg :: RectParams -> Array HSA.PathCommand
nodeBodyBg p =
    [ HSA.m HSA.Abs 0.0 (p.slope * 2.0)
    , HSA.l HSA.Abs p.slope p.slope
    , HSA.l HSA.Abs (p.width * 0.7) p.slope
    , HSA.l HSA.Abs (p.width * 0.7 + p.slope) 0.0
    , HSA.l HSA.Abs (p.width - p.slope) 0.0
    , HSA.l HSA.Abs p.width p.slope
    , HSA.l HSA.Abs p.width (p.height - p.slope)
    , HSA.l HSA.Abs (p.width - p.slope) p.height
    , HSA.l HSA.Abs p.slope p.height
    , HSA.l HSA.Abs 0.0 (p.height - p.slope)
    , HSA.z
    ]


channelBarTop :: RectParams -> Array HSA.PathCommand
channelBarTop p =
    [ HSA.m HSA.Abs 0.0 0.0
    , HSA.l HSA.Abs p.width 0.0
    , HSA.l HSA.Abs p.width p.slope
    , HSA.l HSA.Abs (p.width + p.slope) p.slope
    , HSA.l HSA.Abs (p.width + p.slope) p.height
    , HSA.l HSA.Abs p.width (p.height + p.slope)
    , HSA.l HSA.Abs p.width p.height
    , HSA.l HSA.Abs 0.0 p.height
    , HSA.l HSA.Abs 0.0 (p.height + p.slope)
    , HSA.l HSA.Abs (-p.slope) p.height
    , HSA.l HSA.Abs (-p.slope) p.slope
    , HSA.l HSA.Abs 0.0 p.slope
    , HSA.z
    ]


channelBarBottom :: RectParams -> Array HSA.PathCommand
channelBarBottom p =
    [ HSA.m HSA.Abs 0.0 (-p.slope)
    , HSA.l HSA.Abs 0.0 0.0
    , HSA.l HSA.Abs p.width 0.0
    , HSA.l HSA.Abs p.width (-p.slope)
    , HSA.l HSA.Abs (p.width + p.slope) 0.0
    , HSA.l HSA.Abs (p.width + p.slope) (p.height - p.slope)
    , HSA.l HSA.Abs p.width (p.height - p.slope)
    , HSA.l HSA.Abs p.width p.height
    , HSA.l HSA.Abs 0.0 p.height
    , HSA.l HSA.Abs (-p.slope) (p.height - p.slope)
    , HSA.l HSA.Abs (-p.slope) 0.0
    , HSA.z
    ]


libraryTop :: RectParams -> Array HSA.PathCommand
libraryTop p =
    [ HSA.m HSA.Abs 0.0 p.slope
    , HSA.l HSA.Abs p.slope p.slope
    , HSA.l HSA.Abs (p.slope * 2.0) 0.0
    , HSA.l HSA.Abs (p.width - p.slope * 2.0) 0.0
    , HSA.l HSA.Abs (p.width - p.slope) p.slope
    , HSA.l HSA.Abs p.width p.slope
    , HSA.l HSA.Abs p.width $ p.height + p.slope
    , HSA.l HSA.Abs (p.width - p.slope) p.height
    , HSA.l HSA.Abs (p.width * 0.7 + p.slope) p.height
    , HSA.l HSA.Abs (p.width * 0.7) $ p.height + p.slope
    , HSA.l HSA.Abs p.slope $ p.height + p.slope
    , HSA.l HSA.Abs 0.0 $ p.height + (2.0 * p.slope)
    , HSA.z
    ]


libraryBottom :: RectParams -> Array HSA.PathCommand
libraryBottom p =
    [ HSA.m HSA.Abs 0.0 (-1.0 * p.slope)
    , HSA.l HSA.Abs p.slope 0.0
    , HSA.l HSA.Abs (p.width - p.slope) 0.0
    , HSA.l HSA.Abs p.width (-1.0 * p.slope)
    , HSA.l HSA.Abs p.width $ p.height - p.slope
    , HSA.l HSA.Abs (p.width - p.slope) $ p.height - p.slope
    , HSA.l HSA.Abs (p.width - p.slope * 2.0) p.height
    , HSA.l HSA.Abs (p.slope * 2.0) p.height
    , HSA.l HSA.Abs p.slope $ p.height - p.slope
    , HSA.l HSA.Abs 0.0 $ p.height - p.slope
    , HSA.z
    ]


libraryBody :: RectParams -> Array HSA.PathCommand
libraryBody = nodeBodyBg


statusBar :: RectParams -> Array HSA.PathCommand
statusBar p =
    -- M2.4,0 L10,0 L10,0.9 L9.7,1.2 L1.2,1.2 L0.9,0.9 L0.6,0.9 L0.3,0.6 L1.8,0.6 Z
    [ HSA.m HSA.Abs (p.width * 0.3) 0.0
    , HSA.l HSA.Abs p.width 0.0
    , HSA.l HSA.Abs p.width (p.height - p.slope)
    , HSA.l HSA.Abs (p.width - p.slope) p.height
    , HSA.l HSA.Abs (p.slope * 4.0) p.height
    , HSA.l HSA.Abs (p.slope * 3.0) (p.height - p.slope)
    , HSA.l HSA.Abs (p.slope * 2.0) (p.height - p.slope)
    , HSA.l HSA.Abs (p.slope * 1.0) (p.height - p.slope * 2.0)
    , HSA.l HSA.Abs ((p.width * 0.3) - (p.height - p.slope * 2.0)) (p.height - p.slope * 2.0)
    , HSA.z
    ]


removeButton :: { size :: Number } -> Array HSA.PathCommand
removeButton p =
    [ HSA.m HSA.Abs 0.0 0.0
    , HSA.l HSA.Abs p.size p.size
    , HSA.m HSA.Abs 0.0 p.size
    , HSA.l HSA.Abs p.size 0.0
    ]


panelTop :: RectParams -> Array HSA.PathCommand
panelTop = libraryTop -- reflect bottom part ?


panelBody :: RectParams -> Array HSA.PathCommand
panelBody = libraryBody -- reflect top part ?