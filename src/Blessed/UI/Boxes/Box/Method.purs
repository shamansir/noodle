module Blessed.UI.Boxes.Box.Method where

import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.JsApi (NodeId) as C
import Blessed.Internal.Core (method) as C


-- FIXME: Methods from Element


setContent :: forall m. String -> C.NodeId -> BlessedOp m
setContent value nodeId =
    C.method nodeId "setContent"
        [ C.arg CA.string value
        ]


setLine :: forall m. Int -> String -> C.NodeId -> BlessedOp m
setLine n value nodeId =
    C.method nodeId "setLine"
        [ C.arg CA.int n
        , C.arg CA.string value
        ]


insertLine :: forall m. Int -> String -> C.NodeId -> BlessedOp m
insertLine n value nodeId =
    C.method nodeId "insertLine"
        [ C.arg CA.int n
        , C.arg CA.string value
        ]


focus :: forall m. C.NodeId -> BlessedOp m
focus nodeId =
    C.method nodeId "focus" []


{-
setContent value:String
setLine n:Int value:String
insertLine n:Int value:String
focus
-}