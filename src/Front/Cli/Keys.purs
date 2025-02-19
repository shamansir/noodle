module Cli.Keys where


import Blessed.Internal.BlessedSubj (Screen, ListBar, Box, List, Line, Button, TextBox, Log)
import Blessed.Internal.NodeKey (nk, type (<^>))


type InletsBoxKey = Box <^> "node-inlets-box"
type InfoBoxKey = Box <^> "node-info-box"
type OutletsBoxKey = Box <^> "node-outlets-box"
type NodeBoxKey = Box <^> "node-box"
type PatchBoxKey = Box <^> "patch-box"
type InletButtonKey  = Button <^> "inlet-button"
type OutletButtonKey = Button <^> "outlet-button"
type StatusLineKey = Box <^> "status-line"
type RemoveButtonKey = Button <^> "remove-button"
type CommandInputKey = TextBox <^> "command-input"


type ValueEditorKey sym = TextBox <^> sym
type TextValueEditorKey = ValueEditorKey "text-value-editor"
type NumberValueEditorKey = ValueEditorKey "number-value-editor"
type ColorValueEditorKey = ValueEditorKey "color-value-editor"


mainScreen = nk :: Screen <^> "main-scr"
patchesBar = nk :: ListBar <^> "patches-bar"
patchBox = nk :: PatchBoxKey
library = nk :: List <^> "library"
statusLine = nk :: StatusLineKey
nodeBox = nk :: NodeBoxKey
inletsBox = nk :: InletsBoxKey
outletsBox = nk :: OutletsBoxKey
infoBox = nk :: InfoBoxKey
inletButton = nk :: InletButtonKey
outletButton = nk :: OutletButtonKey
removeButton = nk :: RemoveButtonKey
addPatchButton = nk :: Button <^> "add-patch"
commandInput = nk :: TextBox <^> "command-input"
-- loadFileButton = nk :: Button <^> "load-file"
commandLogButton = nk :: Button <^> "command-log"
commandLogBox = nk :: Log <^> "command-log"
consoleButton = nk :: Button <^> "console"
consoleBox = nk :: Log <^> "console"
hydraCodeButton = nk :: Button <^> "hydra-code"
hydraCodeBox = nk :: Log <^> "hydra-code"
documentationButton = nk :: Button <^> "documentation"
documentationBox = nk :: Log <^> "documentation"
wsStatusButton = nk :: Button <^> "ws-status"
wsStatusBox = nk :: Log <^> "ws-status"
inletIndicator = nk :: Button <^> "inlet-led"
outletIndicator = nk :: Button <^> "outlet-led"


textValueEditor = nk :: TextValueEditorKey
numberValueEditor = nk :: NumberValueEditorKey
colorValueEditor = nk :: ColorValueEditorKey


{- For links -}


type LineA = Line <^> "line-a"
type LineB = Line <^> "line-b"
type LineC = Line <^> "line-c"


lineA = nk :: LineA
lineB = nk :: LineB
lineC = nk :: LineC