module Noodle.Ui.Cli.Palette where

import Noodle.Ui.Cli.Palette.Item (Item, reprName, reprOf, rgb)
import Noodle.Ui.Cli.Palette.Set.Pico8 as Pico
import Noodle.Ui.Cli.Palette.Set.X11 as X11


networkBg = reprName "#111" "background" :: Item
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
focusedBorder = reprName "white" "focusedBorder" :: Item
fg = Pico.white :: Item
secondaryFg = Pico.lightGrey :: Item
patchBg = Pico.darkerGrey :: Item
inletId = X11.bisque3 :: Item
outletId = X11.cadetblue2 :: Item
inletIdx = X11.greenyellow :: Item
outletIdx = X11.green3 :: Item
someGroup = Pico.green :: Item -- FIXME: use `mark`
familyName = Pico.blue :: Item -- FIXME: use `mark`
operator = Pico.blueGreen :: Item -- FIXME: use `mark`
comment = Pico.darkGrey :: Item -- FIXME: use `mark`
documentation = Pico.darkBrown :: Item -- FIXME: use `mark`
nodeId = Pico.lavender :: Item -- FIXME: use `mark`
coord = Pico.green :: Item -- FIXME: use `mark`
value = Pico.brown :: Item -- FIXME: use `mark`
toolkit = Pico.orange :: Item -- FIXME: use `mark`
tkVersion = Pico.darkPeach :: Item -- FIXME: use `mark`
ndfVersion = Pico.mediumGreen :: Item -- FIXME: use `mark`
outletConnect = Pico.orange :: Item
outlet = Pico.brown :: Item
inletHot = Pico.darkRed :: Item
inletCold = Pico.trueBlue :: Item
positive = Pico.lightYellow :: Item
neutral = Pico.lightGrey :: Item
negative = Pico.darkRed :: Item
nodeHlBg = Pico.mauve :: Item
groupBg = Pico.blue :: Item
type_ = Pico.pink :: Item
orderItem = Pico.blue :: Item
orderSplit = Pico.brown :: Item
filePath = Pico.darkerPurple :: Item


networkBg' = reprOf networkBg :: String
itemNotSelected' = reprOf itemNotSelected :: String
itemSelected' = reprOf itemSelected :: String
border' = reprOf border :: String
libraryBg' = reprOf libraryBg :: String
libraryFg' = reprOf libraryFg :: String
libraryBorder' = reprOf libraryBorder :: String
librarySelection' = reprOf librarySelection :: String
nodeBg' = reprOf nodeBg :: String
nodeFg' = reprOf nodeFg :: String
nodeBorder' = reprOf nodeBorder :: String
nodeSelection' = reprOf nodeSelection :: String
linkFg' = reprOf linkFg :: String
focusedBorder' = reprOf focusedBorder :: String
fg' = reprOf fg :: String
secondaryFg' = reprOf secondaryFg :: String
patchBg' = reprOf patchBg :: String
nodeHlBg' = reprOf nodeHlBg :: String


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
    -- FIXME: add all
    ]
