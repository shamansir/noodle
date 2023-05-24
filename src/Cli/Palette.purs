module Cli.Palette where


import Cli.Palette.Item (Item, qitem, repr, rgb)
import Cli.Palette.Set.Pico8 as Pico
import Cli.Palette.Set.X11 as X11


networkBackground = qitem "#111" "background" :: Item
itemNotSelected = qitem "#006600" "itemNotSelected" :: Item
itemSelected = qitem "#00ff00" "itemSelected" :: Item
-- border = X11.darkgray :: Item
border = Pico.lavender :: Item
nodeListFg = qitem "#666" "nodeListFg" :: Item
nodeListSelFg = qitem "white" "nodeListSelFg" :: Item
nodeBoxBorder = qitem "blue" "nodeBoxBorder" :: Item
familyMarker = qitem "#000033" "familyMarker" :: Item
linkColor = qitem "green" "linkColor" :: Item
focusedBorder = qitem "white" "focusedBorder" :: Item
foreground = qitem "white" "foreground" :: Item
patchBackground = Pico.darkerGrey :: Item


networkBackground' = repr networkBackground :: String
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
patchBackground' = repr patchBackground :: String


asArray :: Array Item
asArray =
    [ networkBackground
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
    , patchBackground
    ]
