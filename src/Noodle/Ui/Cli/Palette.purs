module Noodle.Ui.Cli.Palette where

import Noodle.Ui.Cli.Palette.Item (Item, qitem, repr, rgb)
import Noodle.Ui.Cli.Palette.Set.Pico8 as Pico
import Noodle.Ui.Cli.Palette.Set.X11 as X11


networkBg = qitem "#111" "background" :: Item
itemNotSelected = Pico.trueBlue :: Item
itemSelected = Pico.blue :: Item
-- border = X11.darkgray :: Item
border = Pico.lavender :: Item
libraryBg = Pico.brownishBlack :: Item
libraryFg = Pico.white :: Item
libraryBorder = Pico.brownishBlack :: Item
librarySelection = Pico.lightPeach :: Item
nodeBg = Pico.brownishBlack :: Item
nodeFg = Pico.white :: Item
nodeBorder = X11.gray25 :: Item
nodeSelection = Pico.lightPeach :: Item
linkFg = Pico.lightPeach :: Item
focusedBorder = qitem "white" "focusedBorder" :: Item
fg = Pico.white :: Item
secondaryFg = Pico.lightGrey :: Item
patchBg = Pico.darkerGrey :: Item
inletId = X11.bisque3 :: Item
outletId = X11.cadetblue2 :: Item
inletIdx = X11.greenyellow :: Item
outletIdx = X11.green3 :: Item
familyName = Pico.blue :: Item -- FIXME: use `mark`
operator = Pico.blueGreen :: Item -- FIXME: use `mark`
comment = Pico.darkGrey :: Item -- FIXME: use `mark`
nodeId = Pico.lavender :: Item -- FIXME: use `mark`
coord = Pico.green :: Item -- FIXME: use `mark`
value = Pico.brown :: Item -- FIXME: use `mark`
toolkit = Pico.orange :: Item -- FIXME: use `mark`
version = Pico.darkPeach :: Item -- FIXME: use `mark`
positive = Pico.lightYellow :: Item
neutral = Pico.lightGrey :: Item
negative = Pico.darkRed :: Item
nodeHlBg = Pico.mauve :: Item
groupBg = Pico.blue :: Item


networkBg' = repr networkBg :: String
itemNotSelected' = repr itemNotSelected :: String
itemSelected' = repr itemSelected :: String
border' = repr border :: String
libraryBg' = repr libraryBg :: String
libraryFg' = repr libraryFg :: String
libraryBorder' = repr libraryBorder :: String
librarySelection' = repr librarySelection :: String
nodeBg' = repr nodeBg :: String
nodeFg' = repr nodeFg :: String
nodeBorder' = repr nodeBorder :: String
nodeSelection' = repr nodeSelection :: String
linkFg' = repr linkFg :: String
focusedBorder' = repr focusedBorder :: String
fg' = repr fg :: String
secondaryFg' = repr secondaryFg :: String
patchBg' = repr patchBg :: String
nodeHlBg' = repr nodeHlBg :: String


asArray :: Array Item
asArray =
    [ networkBg
    , itemNotSelected
    , itemSelected
    , border
    , libraryBg
    , libraryFg
    , libraryBorder
    , librarySelection
    , nodeBg
    , nodeFg
    , nodeBorder
    , nodeSelection
    , linkFg
    , focusedBorder
    , fg
    , patchBg
    ]
