module Blessed.UI.Base.Element.Method.Content where



import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.JsApi (NodeId) as C
import Blessed.Internal.Core (method) as C



setContent :: forall m. String -> C.NodeId -> BlessedOp m
setContent text nodeId =
    C.method nodeId "setContent" [ C.arg CA.string text ]



getContent :: forall m. C.NodeId -> BlessedOp m
getContent nodeId =
    C.method nodeId "getContent" [ ]



setText :: forall m. String -> C.NodeId -> BlessedOp m
setText text nodeId =
    C.method nodeId "setText" [ C.arg CA.string text ]



getText :: forall m. C.NodeId -> BlessedOp m
getText nodeId =
    C.method nodeId "getText" [ ]



insertLine :: forall m. Int -> String -> C.NodeId -> BlessedOp m
insertLine i line nodeId =
    C.method nodeId "insertLine" [ C.arg CA.int i, C.arg CA.string line ]



insertLines :: forall m. Int -> Array String -> C.NodeId -> BlessedOp m
insertLines i lines nodeId =
    C.method nodeId "insertLines" [ C.arg CA.int i, C.arg (CA.array CA.string) lines ]



deleteLine :: forall m. Int -> C.NodeId -> BlessedOp m
deleteLine i nodeId =
    C.method nodeId "deleteLine" [ C.arg CA.int i ]



getLine :: forall m. Int -> C.NodeId -> BlessedOp m
getLine i nodeId =
    C.method nodeId "getLine" [ C.arg CA.int i ]



getBaseLine :: forall m. Int -> C.NodeId -> BlessedOp m
getBaseLine i nodeId =
    C.method nodeId "getBaseLine" [ C.arg CA.int i ]



setLine :: forall m. Int -> String -> C.NodeId -> BlessedOp m
setLine i line nodeId =
    C.method nodeId "setLine" [ C.arg CA.int i, C.arg CA.string line ]



setBaseLine :: forall m. Int -> String -> C.NodeId -> BlessedOp m
setBaseLine i line nodeId =
    C.method nodeId "setBaseLine" [ C.arg CA.int i, C.arg CA.string line ]



clearLine :: forall m. Int -> C.NodeId -> BlessedOp m
clearLine i nodeId =
    C.method nodeId "clearLine" [ C.arg CA.int i ]



clearBaseLine :: forall m. Int -> C.NodeId -> BlessedOp m
clearBaseLine i nodeId =
    C.method nodeId "clearBaseLine" [ C.arg CA.int i ]



insertTop :: forall m. String -> C.NodeId -> BlessedOp m
insertTop line nodeId =
    C.method nodeId "insertTop" [ C.arg CA.string line ]



insertTops :: forall m. Array String -> C.NodeId -> BlessedOp m
insertTops lines nodeId =
    C.method nodeId "insertTop" [ C.arg (CA.array CA.string) lines ]



insertBottom :: forall m. String -> C.NodeId -> BlessedOp m
insertBottom line nodeId =
    C.method nodeId "insertBottom" [ C.arg CA.string line ]



insertBottoms :: forall m. Array String -> C.NodeId -> BlessedOp m
insertBottoms lines nodeId =
    C.method nodeId "insertBottom" [ C.arg (CA.array CA.string) lines ]



deleteTop :: forall m. C.NodeId -> BlessedOp m
deleteTop nodeId =
    C.method nodeId "deleteTop" [ ]



deleteBottom :: forall m. C.NodeId -> BlessedOp m
deleteBottom nodeId =
    C.method nodeId "deleteBottom" [ ]



unshiftLine :: forall m. String -> C.NodeId -> BlessedOp m
unshiftLine line nodeId =
    C.method nodeId "unshiftLine" [ C.arg CA.string line ]


unshiftLines :: forall m. Array String -> C.NodeId -> BlessedOp m
unshiftLines lines nodeId =
    C.method nodeId "unshiftLine" [ C.arg (CA.array CA.string) lines ]



shiftLine :: forall m. Int -> C.NodeId -> BlessedOp m
shiftLine i nodeId =
    C.method nodeId "shiftLine" [ C.arg CA.int i ]



pushLine :: forall m. String -> C.NodeId -> BlessedOp m
pushLine line nodeId =
    C.method nodeId "pushLine" [ C.arg CA.string line ]



pushLines :: forall m. Array String -> C.NodeId -> BlessedOp m
pushLines lines nodeId =
    C.method nodeId "pushLine" [ C.arg (CA.array CA.string) lines ]



popLine :: forall m. Int -> C.NodeId -> BlessedOp m
popLine i nodeId =
    C.method nodeId "popLine" [ C.arg CA.int i ]



getLines :: forall m. C.NodeId -> BlessedOp m
getLines nodeId =
    C.method nodeId "getLines" [ ]



getScreenLines :: forall m. C.NodeId -> BlessedOp m
getScreenLines nodeId =
    C.method nodeId "getScreenLines" [ ]



strWidth :: forall m. String -> C.NodeId -> BlessedOp m
strWidth text nodeId =
    C.method nodeId "strWidth" [ C.arg CA.string text ]








{-
setContent text:String
getContent
setText text:String
getText
insertLine i:Int line:String
insertLines i:Int lines:ArrayString
deleteLine i:Int
getLine i:Int
getBaseLine i:Int
setLine i:Int line:String
setBaseLine i:Int line:String
clearLine i:Int
clearBaseLine i:Int
insertTop line:String
insertTops lines:ArrayString
insertBottom line:String
insertBottoms lines:ArrayString
deleteTop
deleteBottom
unshiftLine lines:ArrayString
shiftLine i:Int
pushLine line:String
pushLines lines:ArrayString
popLine i:Int
getLines
getScreenLines
strWidth text:String
-}