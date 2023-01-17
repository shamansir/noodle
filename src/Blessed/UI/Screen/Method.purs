module Blessed.UI.Screen.Method where

import Data.Maybe (Maybe)

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA

import Data.Argonaut.Encode (encodeJson)

import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.Command (arg) as C
import Blessed.Internal.Core (method) as C
import Blessed.Internal.JsApi (NodeId) as C
import Blessed.Core.Color (Color)
import Blessed.Core.Cursor (Cursor)


-- key
-- onceKey
-- unkey


logM :: forall m. String -> C.NodeId -> BlessedOp m
logM msg nodeId =
    C.method nodeId "log"
        [ C.arg CA.string msg
        ]


debugM :: forall m. String -> C.NodeId -> BlessedOp m
debugM msg nodeId =
    C.method nodeId "debug"
        [ C.arg CA.string msg
        ]


alloc :: forall m. C.NodeId -> BlessedOp m
alloc nodeId =
    C.method nodeId "alloc" [  ]


realloc :: forall m. C.NodeId -> BlessedOp m
realloc nodeId =
    C.method nodeId "realloc" [ ]


draw :: forall m. Int -> Int -> C.NodeId -> BlessedOp m
draw start end nodeId =
    C.method nodeId "draw"
        [ C.arg CA.int start
        , C.arg CA.int  end
        ]


render :: forall m. C.NodeId -> BlessedOp m
render nodeId =
    C.method nodeId "render" [ ]


clearRegion :: forall m. Int -> Int -> Int -> Int -> C.NodeId -> BlessedOp m
clearRegion x1 x2 y1 y2 nodeId =
    C.method nodeId "clearRegion"
        [ C.arg CA.int x1, C.arg CA.int x2
        , C.arg CA.int y1, C.arg CA.int y2
        ]


fillRegion :: forall m. String -> Char -> Int -> Int -> Int -> Int -> C.NodeId -> BlessedOp m
fillRegion attr ch x1 x2 y1 y2 nodeId =
    C.method nodeId "fillRegion"
        [ C.arg CA.string attr
        , C.arg CA.char ch
        , C.arg CA.int x1, C.arg CA.int x2
        , C.arg CA.int y1, C.arg CA.int y2
        ]


focusOffset :: forall m. Int -> C.NodeId -> BlessedOp m
focusOffset offset nodeId =
    C.method nodeId "focusOffset"
        [ C.arg CA.int offset
        ]


focusPrevious :: forall m. C.NodeId -> BlessedOp m
focusPrevious nodeId =
    C.method nodeId "focusPrevious" [ ]


focusNext :: forall m. C.NodeId -> BlessedOp m
focusNext nodeId =
    C.method nodeId "focusNext" [ ]


focusPush :: forall m. String -> C.NodeId -> BlessedOp m
focusPush element nodeId =
    C.method nodeId "focusPush"
        [ encodeJson element ]


focusPop :: forall m. C.NodeId -> BlessedOp m
focusPop nodeId =
    C.method nodeId "focusPop" [  ]


saveFocus :: forall m. C.NodeId -> BlessedOp m
saveFocus nodeId =
    C.method nodeId "saveFocus" [  ]


restoreFocus :: forall m. C.NodeId -> BlessedOp m
restoreFocus nodeId =
    C.method nodeId "restoreFocus" [  ]


rewindFocus :: forall m. C.NodeId -> BlessedOp m
rewindFocus nodeId =
    C.method nodeId "rewindFocus" [  ]


spawn :: forall m. String -> Array Json -> Json -> C.NodeId -> BlessedOp m
spawn file args options nodeId  =
    C.method nodeId "spawn"
        [ C.arg CA.string file
        , C.arg (CA.array CA.json) args
        , C.arg CA.json options
        ]


insertLine :: forall m. Int -> Int -> Int -> C.NodeId -> BlessedOp m
insertLine n y top nodeId =
    C.method nodeId "insertLine"
        [ C.arg CA.int n
        , C.arg CA.int y
        , C.arg CA.int top
        ]


deleteLine :: forall m. Int -> Int -> Int -> C.NodeId -> BlessedOp m
deleteLine n y top nodeId =
    C.method nodeId "deleteLine"
        [ C.arg CA.int n
        , C.arg CA.int y
        , C.arg CA.int top
        ]


insertBottom :: forall m. Int -> Int -> C.NodeId -> BlessedOp m
insertBottom top bottom nodeId =
    C.method nodeId "insertBottom"
        [ C.arg CA.int top
        , C.arg CA.int bottom
        ]


insertTop :: forall m. Int -> Int -> C.NodeId -> BlessedOp m
insertTop top bottom nodeId =
    C.method nodeId "insertTop"
        [ C.arg CA.int top
        , C.arg CA.int bottom
        ]


deleteBottom :: forall m. Int -> Int -> C.NodeId -> BlessedOp m
deleteBottom top bottom nodeId =
    C.method nodeId "deleteBottom"
        [ C.arg CA.int top
        , C.arg CA.int bottom
        ]


deleteTop :: forall m. Int -> Int -> C.NodeId -> BlessedOp m
deleteTop top bottom nodeId =
    C.method nodeId "deleteTop"
        [ C.arg CA.int top
        , C.arg CA.int bottom
        ]


enableMouse :: forall m. Maybe C.NodeId -> C.NodeId -> BlessedOp m
enableMouse element nodeId =
    C.method nodeId "enableMouse"
        [ encodeJson element
        ]


enableKeys :: forall m. Maybe C.NodeId -> C.NodeId -> BlessedOp m
enableKeys element nodeId =
    C.method nodeId "enableKeys"
        [ encodeJson element
        ]


enableInput :: forall m. Maybe C.NodeId -> C.NodeId -> BlessedOp m
enableInput element nodeId =
    C.method nodeId "enableInput"
        [ encodeJson element
        ]


copyToClipboard :: forall m. String -> C.NodeId -> BlessedOp m
copyToClipboard text nodeId =
    C.method nodeId "copyToClipboard"
        [ C.arg CA.string text
        ]


cursorShape :: forall m. Cursor -> Boolean -> C.NodeId -> BlessedOp m
cursorShape cursor blink nodeId =
    C.method nodeId "cursorShape"
        [ encodeJson cursor
        , C.arg CA.boolean blink
        ]


cursorColor :: forall m. Color -> C.NodeId -> BlessedOp m
cursorColor color nodeId =
    C.method nodeId "cursorColor"
        [ encodeJson color
        ]


screenshot :: forall m. C.NodeId -> BlessedOp m
screenshot nodeId =
    C.method nodeId "screenshot" [ ]


screenshotArea :: forall m. Int -> Int -> Int -> Int -> C.NodeId -> BlessedOp m
screenshotArea xi xl yi yl nodeId =
    C.method nodeId "screenshot"
        [ C.arg CA.int xi
        , C.arg CA.int xl
        , C.arg CA.int yi
        , C.arg CA.int yl
        ]


destroy :: forall m. C.NodeId -> BlessedOp m
destroy nodeId =
    C.method nodeId "destroy" [ ]


setTerminal :: forall m. String -> C.NodeId -> BlessedOp m
setTerminal term nodeId =
    C.method nodeId "destroysetTerminal"
        [ C.arg CA.string term
        ]


{-
log msg:String
debug msg:String
alloc
realloc
draw start:Int end:Int
render
clearRegion x1:Int x2:Int y1:Int y2:Int
fillRegion attr:String ch:Char x1:Int x2:Int y1:Int y2:Int
focusOffset offset:Int
focusPrevious
focusNext
focusPush element:String
focusPop
saveFocus
restoreFocus
rewindFocus
spawn file:String args:Array options:Unit
insertLine n:Int y:Int top:Int
deleteLine n:Int y:Int top:Int
insertBottom top:Int bottom:Int
insertTop top:Int bottom:Int
deleteBottom top:Int bottom:Int
deleteTop top:Int bottom:Int
enableMouse element:Maybe
enableKeys element:Maybe
enableInput element:Maybe
copyToClipboard text:String
cursorShape cursor:Cursor blink:Boolean
cursorColor color:Color
screenshot
screenshotArea xi:Int xl:Int yi:Int yl:Int
destroy
setTerminal term:String
-}