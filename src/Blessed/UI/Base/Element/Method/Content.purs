module Blessed.UI.Base.Element.Method.Content where



import Data.Codec.Argonaut as CA
import Data.Symbol (class IsSymbol)

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedSubj (class Extends, Element, Subject, class IsSubject)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.Core (method) as C



setContent
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => String -> NodeKey subj id -> BlessedOp state m
setContent text nodeId =
    C.method nodeId "setContent" [ C.arg CA.string text ]



getContent
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
getContent nodeId =
    C.method nodeId "getContent" [ ]



setText
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => String -> NodeKey subj id -> BlessedOp state m
setText text nodeId =
    C.method nodeId "setText" [ C.arg CA.string text ]



getText
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
getText nodeId =
    C.method nodeId "getText" [ ]



insertLine
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Int -> String -> NodeKey subj id -> BlessedOp state m
insertLine i line nodeId =
    C.method nodeId "insertLine" [ C.arg CA.int i, C.arg CA.string line ]



insertLines
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Int -> Array String -> NodeKey subj id -> BlessedOp state m
insertLines i lines nodeId =
    C.method nodeId "insertLines" [ C.arg CA.int i, C.arg (CA.array CA.string) lines ]



deleteLine
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Int -> NodeKey subj id -> BlessedOp state m
deleteLine i nodeId =
    C.method nodeId "deleteLine" [ C.arg CA.int i ]



getLine
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Int -> NodeKey subj id -> BlessedOp state m
getLine i nodeId =
    C.method nodeId "getLine" [ C.arg CA.int i ]



getBaseLine
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Int -> NodeKey subj id -> BlessedOp state m
getBaseLine i nodeId =
    C.method nodeId "getBaseLine" [ C.arg CA.int i ]



setLine
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Int -> String -> NodeKey subj id -> BlessedOp state m
setLine i line nodeId =
    C.method nodeId "setLine" [ C.arg CA.int i, C.arg CA.string line ]



setBaseLine
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Int -> String -> NodeKey subj id -> BlessedOp state m
setBaseLine i line nodeId =
    C.method nodeId "setBaseLine" [ C.arg CA.int i, C.arg CA.string line ]



clearLine
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Int -> NodeKey subj id -> BlessedOp state m
clearLine i nodeId =
    C.method nodeId "clearLine" [ C.arg CA.int i ]



clearBaseLine
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Int -> NodeKey subj id -> BlessedOp state m
clearBaseLine i nodeId =
    C.method nodeId "clearBaseLine" [ C.arg CA.int i ]



insertTop
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => String -> NodeKey subj id -> BlessedOp state m
insertTop line nodeId =
    C.method nodeId "insertTop" [ C.arg CA.string line ]



insertTops
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Array String -> NodeKey subj id -> BlessedOp state m
insertTops lines nodeId =
    C.method nodeId "insertTop" [ C.arg (CA.array CA.string) lines ]



insertBottom
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => String -> NodeKey subj id -> BlessedOp state m
insertBottom line nodeId =
    C.method nodeId "insertBottom" [ C.arg CA.string line ]



insertBottoms
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Array String -> NodeKey subj id -> BlessedOp state m
insertBottoms lines nodeId =
    C.method nodeId "insertBottom" [ C.arg (CA.array CA.string) lines ]



deleteTop
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
deleteTop nodeId =
    C.method nodeId "deleteTop" [ ]



deleteBottom
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
deleteBottom nodeId =
    C.method nodeId "deleteBottom" [ ]


unshiftLine
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => String -> NodeKey subj id -> BlessedOp state m
unshiftLine line nodeId =
    C.method nodeId "unshiftLine" [ C.arg CA.string line ]


unshiftLines
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Array String -> NodeKey subj id -> BlessedOp state m
unshiftLines lines nodeId =
    C.method nodeId "unshiftLine" [ C.arg (CA.array CA.string) lines ]



shiftLine
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Int -> NodeKey subj id -> BlessedOp state m
shiftLine i nodeId =
    C.method nodeId "shiftLine" [ C.arg CA.int i ]



pushLine
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => String -> NodeKey subj id -> BlessedOp state m
pushLine line nodeId =
    C.method nodeId "pushLine" [ C.arg CA.string line ]



pushLines
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Array String -> NodeKey subj id -> BlessedOp state m
pushLines lines nodeId =
    C.method nodeId "pushLine" [ C.arg (CA.array CA.string) lines ]



popLine
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Int -> NodeKey subj id -> BlessedOp state m
popLine i nodeId =
    C.method nodeId "popLine" [ C.arg CA.int i ]



getLines
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
getLines nodeId =
    C.method nodeId "getLines" [ ]



getScreenLines
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
getScreenLines nodeId =
    C.method nodeId "getScreenLines" [ ]



strWidth
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => String -> NodeKey subj id -> BlessedOp state m
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