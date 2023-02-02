module Blessed.UI.Base.Screen.Method where

import Prelude (($), (<$>))

import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol)

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA

import Data.Argonaut.Encode (encodeJson)

import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedSubj (Subject, Screen)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.NodeKey (getId) as NodeKey
import Blessed.Internal.Command (arg) as C
import Blessed.Internal.Core (method) as C

import Blessed.Core.Color (Color)
import Blessed.Core.Cursor (Cursor)


-- key
-- onceKey
-- unkey


logM
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => String -> NodeKey subj id -> BlessedOp state m
logM msg nodeId =
    C.method nodeId "log"
        [ C.arg CA.string msg
        ]


debugM
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => String -> NodeKey subj id -> BlessedOp state m
debugM msg nodeId =
    C.method nodeId "debug"
        [ C.arg CA.string msg
        ]


alloc
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> BlessedOp state m
alloc nodeId =
    C.method nodeId "alloc" [  ]


realloc
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> BlessedOp state m
realloc nodeId =
    C.method nodeId "realloc" [ ]


draw
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => Int -> Int -> NodeKey subj id -> BlessedOp state m
draw start end nodeId =
    C.method nodeId "draw"
        [ C.arg CA.int start
        , C.arg CA.int  end
        ]


render
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> BlessedOp state m
render nodeId =
    C.method nodeId "render" [ ]


clearRegion
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => Int -> Int -> Int -> Int -> NodeKey subj id -> BlessedOp state m
clearRegion x1 x2 y1 y2 nodeId =
    C.method nodeId "clearRegion"
        [ C.arg CA.int x1, C.arg CA.int x2
        , C.arg CA.int y1, C.arg CA.int y2
        ]


fillRegion
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => String -> Char -> Int -> Int -> Int -> Int -> NodeKey subj id -> BlessedOp state m
fillRegion attr ch x1 x2 y1 y2 nodeId =
    C.method nodeId "fillRegion"
        [ C.arg CA.string attr
        , C.arg CA.char ch
        , C.arg CA.int x1, C.arg CA.int x2
        , C.arg CA.int y1, C.arg CA.int y2
        ]


focusOffset
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => Int -> NodeKey subj id -> BlessedOp state m
focusOffset offset nodeId =
    C.method nodeId "focusOffset"
        [ C.arg CA.int offset
        ]


focusPrevious
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> BlessedOp state m
focusPrevious nodeId =
    C.method nodeId "focusPrevious" [ ]


focusNext
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> BlessedOp state m
focusNext nodeId =
    C.method nodeId "focusNext" [ ]


focusPush
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => String -> NodeKey subj id -> BlessedOp state m
focusPush element nodeId =
    C.method nodeId "focusPush"
        [ encodeJson element ]


focusPop
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> BlessedOp state m
focusPop nodeId =
    C.method nodeId "focusPop" [  ]


saveFocus
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> BlessedOp state m
saveFocus nodeId =
    C.method nodeId "saveFocus" [  ]


restoreFocus
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> BlessedOp state m
restoreFocus nodeId =
    C.method nodeId "restoreFocus" [  ]


rewindFocus
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> BlessedOp state m
rewindFocus nodeId =
    C.method nodeId "rewindFocus" [  ]


spawn
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => String -> Array Json -> Json -> NodeKey subj id -> BlessedOp state m
spawn file args options nodeId  =
    C.method nodeId "spawn"
        [ C.arg CA.string file
        , C.arg (CA.array CA.json) args
        , C.arg CA.json options
        ]


insertLine
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => Int -> Int -> Int -> NodeKey subj id -> BlessedOp state m
insertLine n y top nodeId =
    C.method nodeId "insertLine"
        [ C.arg CA.int n
        , C.arg CA.int y
        , C.arg CA.int top
        ]


deleteLine
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => Int -> Int -> Int -> NodeKey subj id -> BlessedOp state m
deleteLine n y top nodeId =
    C.method nodeId "deleteLine"
        [ C.arg CA.int n
        , C.arg CA.int y
        , C.arg CA.int top
        ]


insertBottom
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => Int -> Int -> NodeKey subj id -> BlessedOp state m
insertBottom top bottom nodeId =
    C.method nodeId "insertBottom"
        [ C.arg CA.int top
        , C.arg CA.int bottom
        ]


insertTop
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => Int -> Int -> NodeKey subj id -> BlessedOp state m
insertTop top bottom nodeId =
    C.method nodeId "insertTop"
        [ C.arg CA.int top
        , C.arg CA.int bottom
        ]


deleteBottom
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => Int -> Int -> NodeKey subj id -> BlessedOp state m
deleteBottom top bottom nodeId =
    C.method nodeId "deleteBottom"
        [ C.arg CA.int top
        , C.arg CA.int bottom
        ]


deleteTop
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => Int -> Int -> NodeKey subj id -> BlessedOp state m
deleteTop top bottom nodeId =
    C.method nodeId "deleteTop"
        [ C.arg CA.int top
        , C.arg CA.int bottom
        ]


enableMouse
    :: forall subj' id' (subj :: Subject) (id :: Symbol) state m
     . IsSymbol id' => Respresents Screen subj id
    => Maybe (NodeKey subj' id') -> NodeKey subj id -> BlessedOp state m
enableMouse element nodeId =
    C.method nodeId "enableMouse"
        [ encodeJson $ NodeKey.getId <$> element
        ]


enableKeys
    :: forall subj' id' (subj :: Subject) (id :: Symbol) state m
     . IsSymbol id' => Respresents Screen subj id
    => Maybe (NodeKey subj' id') -> NodeKey subj id -> BlessedOp state m
enableKeys element nodeId =
    C.method nodeId "enableKeys"
        [ encodeJson $ NodeKey.getId <$> element
        ]


enableInput
    :: forall subj' id' (subj :: Subject) (id :: Symbol) state m
     . IsSymbol id' => Respresents Screen subj id
    => Maybe (NodeKey subj' id') -> NodeKey subj id -> BlessedOp state m
enableInput element nodeId =
    C.method nodeId "enableInput"
        [ encodeJson $ NodeKey.getId <$> element
        ]


copyToClipboard
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => String -> NodeKey subj id -> BlessedOp state m
copyToClipboard text nodeId =
    C.method nodeId "copyToClipboard"
        [ C.arg CA.string text
        ]


cursorShape
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => Cursor -> Boolean -> NodeKey subj id -> BlessedOp state m
cursorShape cursor blink nodeId =
    C.method nodeId "cursorShape"
        [ encodeJson cursor
        , C.arg CA.boolean blink
        ]


cursorColor
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => Color -> NodeKey subj id -> BlessedOp state m
cursorColor color nodeId =
    C.method nodeId "cursorColor"
        [ encodeJson color
        ]


screenshot
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> BlessedOp state m
screenshot nodeId =
    C.method nodeId "screenshot" [ ]


screenshotArea
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => Int -> Int -> Int -> Int -> NodeKey subj id -> BlessedOp state m
screenshotArea xi xl yi yl nodeId =
    C.method nodeId "screenshot"
        [ C.arg CA.int xi
        , C.arg CA.int xl
        , C.arg CA.int yi
        , C.arg CA.int yl
        ]


destroy
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> BlessedOp state m
destroy nodeId =
    C.method nodeId "destroy" [ ]


setTerminal
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => String -> NodeKey subj id -> BlessedOp state m
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