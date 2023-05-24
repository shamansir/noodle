module Cli.Palette where


import Cli.Palette.Item (Item, qitem, repr)


background = qitem "#111" "background" :: Item
itemNotSelected = qitem "#006600" "itemNotSelected" :: Item
itemSelected = qitem "#00ff00" "itemSelected" :: Item
border = qitem "#f0f0f0" "border" :: Item
nodeListFg = qitem "#666" "nodeListFg" :: Item
nodeListSelFg = qitem "white" "nodeListSelFg" :: Item
nodeBoxBorder = qitem "blue" "nodeBoxBorder" :: Item
familyMarker = qitem "#000033" "familyMarker" :: Item
linkColor = qitem "green" "linkColor" :: Item
focusedBorder = qitem "white" "focusedBorder" :: Item
foreground = qitem "white" "foreground" :: Item
background2 = qitem "black" "background2" :: Item


background' = repr background :: String
itemNotSelected' = repr itemNotSelected :: String
itemSelected' = repr itemSelected :: String
border' = repr border :: String
nodeListFg' = repr nodeListFg :: String
nodeListSelFg' = repr nodeListSelFg :: String
nodeBoxBorder' = repr nodeBoxBorder :: String
familyMarker' = repr familyMarker :: String
linkColor' = repr linkColor :: String
focusedBorder' = repr focusedBorder :: String
foreground' = repr foreground :: String
background2' = repr background2 :: String


asArray :: Array Item
asArray =
    [ background
    , itemNotSelected
    , itemSelected
    , border
    , nodeListFg
    , nodeListSelFg
    , nodeBoxBorder
    , familyMarker
    , linkColor
    , focusedBorder
    , foreground
    , background2
    ]
