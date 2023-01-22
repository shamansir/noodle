module Blessed.UI.Message.Method where


import Prelude

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.JsApi (NodeId) as C
import Blessed.Internal.Core (method) as C



load :: forall m. String -> C.NodeId -> BlessedOp m
load text nodeId =
    C.method nodeId "load"
        [ C.arg CA.string text
        ]


stop :: forall m. C.NodeId -> BlessedOp m
stop nodeId =
    C.method nodeId "stop" [ ]
