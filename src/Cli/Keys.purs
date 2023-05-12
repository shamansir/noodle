module Cli.Keys where


import Blessed.Internal.BlessedSubj (Screen, ListBar, Box, List, Line, Button)
import Blessed.Internal.NodeKey (nk, type (<^>))


mainScreen = nk :: Screen <^> "main-scr"
patchesBar = nk :: ListBar <^> "patches-bar"
patchBox = nk :: Box <^> "patch-box"
nodeList = nk :: List <^> "node-list"
nodeBox = nk :: Box <^> "node-box"
inletsBar = nk :: ListBar <^> "node-inlets-bar"
outletsBar = nk :: ListBar <^> "node-outlets-bar"
inlets = nk :: ListBar <^> "inlets"
outlets = nk :: ListBar <^> "outlets"
addPatchButton = nk :: Button <^> "add-patch"


{- For links -}


type LineA = Line <^> "line-a"
type LineB = Line <^> "line-b"
type LineC = Line <^> "line-c"


lineA = nk :: LineA
lineB = nk :: LineB
lineC = nk :: LineC


type InletsBarKey = ListBar <^> "node-inlets-bar"
type OutletsBarKey = ListBar <^> "node-outlets-bar"
type NodeBoxKey = Box <^> "node-box"
type PatchBoxKey = Box <^> "patch-box"