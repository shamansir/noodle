module Cli.Keys where


import Blessed.Internal.BlessedSubj (Screen, ListBar, Box, List, Line, Button, TextBox)
import Blessed.Internal.NodeKey (nk, type (<^>))


type InputsBoxKey = Box <^> "node-inputs-box"
type InfoBoxKey = Box <^> "node-info-box"
type OutputsBoxKey = Box <^> "node-outputs-box"
type NodeBoxKey = Box <^> "node-box"
type PatchBoxKey = Box <^> "patch-box"
type InputButtonKey = Button <^> "input-button"
type OutputButtonKey = Button <^> "output-button"
type StatusLineKey = Box <^> "status-line"
type RemoveButtonKey = Button <^> "remove-button"
-- type TextBoxKey = TextBox <^> "text-box"
type NumValueEditorKey = TextBox <^> "num-value-editor"


mainScreen = nk :: Screen <^> "main-scr"
patchesBar = nk :: ListBar <^> "patches-bar"
patchBox = nk :: PatchBoxKey
library = nk :: List <^> "library"
statusLine = nk :: StatusLineKey
nodeBox = nk :: NodeBoxKey
inputsBox = nk :: InputsBoxKey
outputsBox = nk :: OutputsBoxKey
infoBox = nk :: InfoBoxKey
inputButton = nk :: InputButtonKey
outputButton = nk :: OutputButtonKey
removeButton = nk :: RemoveButtonKey
addPatchButton = nk :: Button <^> "add-patch"
loadFileButton = nk :: Button <^> "load-file"
commandLogButton = nk :: Button <^> "toggle-command-log"
commandLogBox = nk :: Box <^> "command-log"
hydraCodeButton = nk :: Button <^> "toggle-hydra-code"
hydraCodeBox = nk :: Box <^> "hydra-code"
fullInfoButton = nk :: Button <^> "toggle-full-info"
fullInfoBox = nk :: Box <^> "full-info"
wsStatusButton = nk :: Button <^> "ws-status"
numValueEditor = nk :: NumValueEditorKey
inputIndicator = nk :: Button <^> "input-led"
outputIndicator = nk :: Button <^> "output-led"


{- For links -}


type LineA = Line <^> "line-a"
type LineB = Line <^> "line-b"
type LineC = Line <^> "line-c"


lineA = nk :: LineA
lineB = nk :: LineB
lineC = nk :: LineC