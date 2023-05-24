module Cli.Palette where


import Cli.Palette.Item (Item, qitem)


type Palette =
    { background :: Item
    , background2 :: Item
    , border :: Item
    , familyMarker :: Item
    , focusedBorder :: Item
    , foreground :: Item
    , itemNotSelected :: Item
    , itemSelected :: Item
    , linkColor :: Item
    , nodeBoxBorder :: Item
    , nodeListFg :: Item
    , nodeListSelFg :: Item
    }


palette :: Palette
palette =
    { background : qitem "#111" "background" -- 0
    , itemNotSelected : qitem "#006600" "itemNotSelected" -- 1
    , itemSelected : qitem "#00ff00" "itemSelected" -- 2
    , border : qitem "#f0f0f0" "border" -- 3
    , nodeListFg : qitem "#666" "nodeListFg" -- 4
    , nodeListSelFg : qitem "white" "nodeListSelFg" -- 5
    , nodeBoxBorder : qitem "blue" "nodeBoxBorder" -- 6
    , familyMarker : qitem "#000033" "familyMarker" -- 7
    , linkColor : qitem "green" "linkColor" -- 8
    , focusedBorder : qitem "white" "focusedBorder"
    , foreground : qitem "white" "foreground"
    , background2 : qitem "black" "background2"
    }


toArray :: Palette -> Array Item
toArray p =
    [ p.background
    , p.itemNotSelected
    , p.itemSelected
    , p.border
    , p.nodeListFg
    , p.nodeListSelFg
    , p.nodeBoxBorder
    , p.familyMarker
    , p.linkColor
    , p.focusedBorder
    , p.foreground
    , p.background2
    ]
