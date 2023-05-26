module Cli.Keys where


import Blessed.Internal.BlessedSubj (Screen, ListBar, Box, List, Line, Button)
import Blessed.Internal.NodeKey (nk, type (<^>))


mainScreen = nk :: Screen <^> "main-scr"
patchesBar = nk :: ListBar <^> "patches-bar"
patchBox = nk :: Box <^> "patch-box"
library = nk :: List <^> "library"
nodeBox = nk :: Box <^> "node-box"
inletsBox = nk :: Box <^> "node-inlets-box"
outletsBox = nk :: Box <^> "node-outlets-box"
inletButton = nk :: Button <^> "inlet-button"
outletButton = nk :: Button <^> "outlet-button"
addPatchButton = nk :: Button <^> "add-patch"


{- For links -}


type LineA = Line <^> "line-a"
type LineB = Line <^> "line-b"
type LineC = Line <^> "line-c"


lineA = nk :: LineA
lineB = nk :: LineB
lineC = nk :: LineC


type InletsBoxKey = Box <^> "node-inlets-box"
type OutletsBoxKey = Box <^> "node-outlets-box"
type NodeBoxKey = Box <^> "node-box"
type PatchBoxKey = Box <^> "patch-box"