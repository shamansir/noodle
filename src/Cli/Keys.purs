module Cli.Keys where


import Blessed.Internal.BlessedSubj (Screen, ListBar, Box, List, Line, Button, TextBox)
import Blessed.Internal.NodeKey (nk, type (<^>))


type InletsBoxKey = Box <^> "node-inlets-box"
type InfoBoxKey = Box <^> "node-info-box"
type OutletsBoxKey = Box <^> "node-outlets-box"
type NodeBoxKey = Box <^> "node-box"
type PatchBoxKey = Box <^> "patch-box"
type InletButtonKey = Button <^> "inlet-button"
type OutletButtonKey = Button <^> "outlet-button"
-- type TextBoxKey = TextBox <^> "text-box"


mainScreen = nk :: Screen <^> "main-scr"
patchesBar = nk :: ListBar <^> "patches-bar"
patchBox = nk :: PatchBoxKey
library = nk :: List <^> "library"
nodeBox = nk :: NodeBoxKey
inletsBox = nk :: InletsBoxKey
outletsBox = nk :: OutletsBoxKey
infoBox = nk :: InfoBoxKey
inletButton = nk :: InletButtonKey
outletButton = nk :: OutletButtonKey
addPatchButton = nk :: Button <^> "add-patch"
-- textBox = nk :: TextBoxKey


{- For links -}


type LineA = Line <^> "line-a"
type LineB = Line <^> "line-b"
type LineC = Line <^> "line-c"


lineA = nk :: LineA
lineB = nk :: LineB
lineC = nk :: LineC