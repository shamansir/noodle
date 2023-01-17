module Blessed.UI.Screen.Method where

import Prelude

import Data.Maybe (Maybe(..))

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Data.Argonaut.Encode (class EncodeJson, encodeJson)

import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedOp (perform) as Op
import Blessed.Internal.Command (Command, call, arg) as C
import Blessed.Internal.JsApi (NodeId) as C
import Blessed.Core.Color (Color)
import Blessed.Core.Cursor (Cursor)

import Node.Stream (destroy)


-- key
-- onceKey
-- unkey


logM :: forall m. C.NodeId -> String -> BlessedOp m
logM nodeId msg =
    Op.perform nodeId $ C.call "log"
        [ CA.encode CA.string msg
        ]


debugM :: forall m. C.NodeId -> String -> BlessedOp m
debugM nodeId msg =
    Op.perform nodeId $ C.call "debug"
        [ CA.encode CA.string msg
        ]


alloc :: forall m. C.NodeId -> BlessedOp m
alloc nodeId =
    Op.perform nodeId $ C.call "alloc" [  ]


realloc :: forall m. C.NodeId -> BlessedOp m
realloc nodeId =
    Op.perform nodeId $ C.call "realloc" [ ]


draw :: forall m. C.NodeId -> Int -> Int -> BlessedOp m
draw nodeId start end =
    Op.perform nodeId $ C.call "draw"
        [ encodeJson start
        , encodeJson end
        ]


render :: forall m. C.NodeId -> BlessedOp m
render nodeId =
    Op.perform nodeId $ C.call "render" [ ]


clearRegion :: forall m. C.NodeId -> Int -> Int -> Int -> Int -> BlessedOp m
clearRegion nodeId x1 x2 y1 y2 =
    Op.perform nodeId $ C.call "clearRegion"
        [ encodeJson x1, encodeJson x2
        , encodeJson y1, encodeJson y2
        ]


fillRegion :: forall m. C.NodeId -> String -> Char -> Int -> Int -> Int -> Int -> BlessedOp m
fillRegion nodeId attr ch x1 x2 y1 y2 =
    Op.perform nodeId $ C.call "fillRegion"
        [ encodeJson attr
        , encodeJson ch
        , encodeJson x1, encodeJson x2
        , encodeJson y1, encodeJson y2
        ]


focusOffset :: forall m. C.NodeId -> Int -> BlessedOp m
focusOffset nodeId offset =
    Op.perform nodeId $ C.call "focusOffset"
        [ encodeJson offset
        ]


focusPrevious :: forall m. C.NodeId -> BlessedOp m
focusPrevious nodeId =
    Op.perform nodeId $ C.call "focusPrevious" [ ]


focusNext :: forall m. C.NodeId -> BlessedOp m
focusNext nodeId =
    Op.perform nodeId $ C.call "focusNext" [ ]


focusPush :: forall m. C.NodeId -> String -> BlessedOp m
focusPush nodeId element =
    Op.perform nodeId $ C.call "focusPush"
        [ encodeJson element ]


focusPop :: forall m. C.NodeId -> BlessedOp m
focusPop nodeId =
    Op.perform nodeId $ C.call "focusPop" [  ]


saveFocus :: forall m. C.NodeId -> BlessedOp m
saveFocus nodeId =
    Op.perform nodeId $ C.call "saveFocus" [  ]


restoreFocus :: forall m. C.NodeId -> BlessedOp m
restoreFocus nodeId =
    Op.perform nodeId $ C.call "restoreFocus" [  ]


rewindFocus :: forall m. C.NodeId -> BlessedOp m
rewindFocus nodeId =
    Op.perform nodeId $ C.call "rewindFocus" [  ]


spawn :: forall m. C.NodeId -> String -> Array Json -> Unit -> BlessedOp m
spawn nodeId file args options =
    Op.perform nodeId $ C.call "spawn"
        [ encodeJson file
        , encodeJson args
        , encodeJson options
        ]


insertLine :: forall m. C.NodeId -> Int -> Int -> Int -> BlessedOp m
insertLine nodeId n y top =
    Op.perform nodeId $ C.call "insertLine"
        [ encodeJson n
        , encodeJson y
        , encodeJson top
        ]


deleteLine :: forall m. C.NodeId -> Int -> Int -> Int -> BlessedOp m
deleteLine nodeId n y top =
    Op.perform nodeId $ C.call "deleteLine"
        [ encodeJson n
        , encodeJson y
        , encodeJson top
        ]


insertBottom :: forall m. C.NodeId -> Int -> Int -> BlessedOp m
insertBottom nodeId top bottom =
    Op.perform nodeId $ C.call "insertBottom"
        [ encodeJson top
        , encodeJson bottom
        ]


insertTop :: forall m. C.NodeId -> Int -> Int -> BlessedOp m
insertTop nodeId top bottom =
    Op.perform nodeId $ C.call "insertTop"
        [ encodeJson top
        , encodeJson bottom
        ]


deleteBottom :: forall m. C.NodeId -> Int -> Int -> BlessedOp m
deleteBottom nodeId top bottom =
    Op.perform nodeId $ C.call "deleteBottom"
        [ encodeJson top
        , encodeJson bottom
        ]


deleteTop :: forall m. C.NodeId -> Int -> Int -> BlessedOp m
deleteTop nodeId top bottom =
    Op.perform nodeId $ C.call "deleteTop"
        [ encodeJson top
        , encodeJson bottom
        ]


enableMouse :: forall m. C.NodeId -> Maybe C.NodeId -> BlessedOp m
enableMouse nodeId element =
    Op.perform nodeId $ C.call "enableMouse"
        [ encodeJson element
        ]


enableKeys :: forall m. C.NodeId -> Maybe C.NodeId -> BlessedOp m
enableKeys nodeId element =
    Op.perform nodeId $ C.call "enableKeys"
        [ encodeJson element
        ]


enableInput :: forall m. C.NodeId -> Maybe C.NodeId ->BlessedOp m
enableInput nodeId element =
    Op.perform nodeId $ C.call "enableInput"
        [ encodeJson element
        ]


copyToClipboard :: forall m. C.NodeId -> String -> BlessedOp m
copyToClipboard nodeId text =
    Op.perform nodeId $ C.call "copyToClipboard"
        [ encodeJson text
        ]


cursorShape :: forall m. C.NodeId -> Cursor -> Boolean -> BlessedOp m
cursorShape nodeId cursor blink =
    Op.perform nodeId $ C.call "cursorShape"
        [ encodeJson cursor
        , encodeJson blink
        ]


cursorColor :: forall m. C.NodeId -> Color -> BlessedOp m
cursorColor nodeId color =
    Op.perform nodeId $ C.call "cursorColor"
        [ encodeJson color
        ]


screenshot :: forall m. C.NodeId -> BlessedOp m
screenshot nodeId =
    Op.perform nodeId $ C.call "screenshot" [ ]


screenshotArea :: forall m. C.NodeId -> Int -> Int -> Int -> Int -> BlessedOp m
screenshotArea nodeId xi xl yi yl =
    Op.perform nodeId $ C.call "screenshot"
        [ encodeJson xi
        , encodeJson xl
        , encodeJson yi
        , encodeJson yl
        ]


destroy :: forall m. C.NodeId -> BlessedOp m
destroy nodeId =
    Op.perform nodeId $ C.call "destroy" [ ]


setTerminal :: forall m. C.NodeId -> String -> BlessedOp m
setTerminal nodeId term =
    Op.perform nodeId $ C.call "destroysetTerminal"
        [ encodeJson term
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