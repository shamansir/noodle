module Blessed.UI.Box.Method where

import Prelude (($))

import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (call, arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.JsApi (NodeId) as C
import Blessed.Internal.BlessedOp (perform) as Op


setContent :: forall m. String -> C.NodeId -> BlessedOp m
setContent value nodeId =
    Op.perform nodeId
        $ C.call "setContent"
            [ C.arg CA.string value
            ]


setLine :: forall m. Int -> String -> C.NodeId -> BlessedOp m
setLine n value nodeId =
    Op.perform nodeId
        $ C.call "setLine"
            [ C.arg CA.int n
            , C.arg CA.string value
            ]


insertLine :: forall m. Int -> String -> C.NodeId -> BlessedOp m
insertLine n value nodeId =
    Op.perform nodeId
        $ C.call "insertLine"
            [ C.arg CA.int n
            , C.arg CA.string value
            ]


focus :: forall m. C.NodeId -> BlessedOp m
focus nodeId =
    Op.perform nodeId
        $ C.call "focus" []