module Blessed.UI.Node.Method where


import Data.Maybe (Maybe)

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.JsApi (NodeId) as C
import Blessed.Internal.Core (method) as C


prepend :: forall m. String -> C.NodeId -> BlessedOp m
prepend node nodeId =
    C.method nodeId "prepend" [ C.arg CA.string node ]


append :: forall m. String -> C.NodeId -> BlessedOp m
append node nodeId =
    C.method nodeId "append" [ C.arg CA.string node ]


remove :: forall m. String -> C.NodeId -> BlessedOp m
remove node nodeId =
    C.method nodeId "remove" [ C.arg CA.string node ]


insert :: forall m. String -> Int -> C.NodeId -> BlessedOp m
insert node i nodeId =
    C.method nodeId "insert" [ C.arg CA.string node, C.arg CA.int i ]


insertBefore :: forall m. String -> String -> C.NodeId -> BlessedOp m
insertBefore node refNode nodeId =
    C.method nodeId "insertBefore" [ C.arg CA.string node, C.arg CA.string refNode ]


insertAfter :: forall m. String -> String -> C.NodeId -> BlessedOp m
insertAfter node refNode nodeId =
    C.method nodeId "insertAfter" [ C.arg CA.string node, C.arg CA.string refNode ]


detach :: forall m.  C.NodeId -> BlessedOp m
detach nodeId =
    C.method nodeId "detach" [ ]


emitDescandants :: forall m.  C.NodeId -> BlessedOp m
emitDescandants nodeId =
    C.method nodeId "emitDescandants" [ ]


get :: forall m. String -> Maybe Json -> C.NodeId -> BlessedOp m -- FIXME
get name value nodeId =
    C.method nodeId "get" [ C.arg CA.string name, C.arg (CAC.maybe CA.json) value ]


set :: forall m. String -> Json -> C.NodeId -> BlessedOp m
set name value nodeId =
    C.method nodeId "set" [ C.arg CA.string name, C.arg CA.json value ]


{-
prepend node:String
append node:String
remove node:String
insert node:String i:Int
insertBefore node:String refNode:String
insertAfter node:String refNode:String
detach
emitDescandants
get name:String value:MaybeJson
set name:String value:Json
-}