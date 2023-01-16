module Blessed.UI.Box.Method where

import Prelude (($))

import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (call, arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.JsApi (NodeId) as C
import Blessed.Internal.BlessedOp (perform) as Op


setContent :: forall m. C.NodeId -> String -> BlessedOp m
setContent nodeId value =
    Op.perform nodeId
        $ C.call "setContent"
            [ C.arg CA.string value
            ]


setLine :: forall m. C.NodeId -> Int -> String -> BlessedOp m
setLine nodeId n value =
    Op.perform nodeId
        $ C.call "setLine"
            [ C.arg CA.int n
            , C.arg CA.string value
            ]


insertLine :: forall m. C.NodeId -> Int -> String -> BlessedOp m
insertLine nodeId n value =
    Op.perform nodeId
        $ C.call "insertLine"
            [ C.arg CA.int n
            , C.arg CA.string value
            ]


focus :: forall m. C.NodeId -> BlessedOp m
focus nodeId =
    Op.perform nodeId
        $ C.call "focus" []