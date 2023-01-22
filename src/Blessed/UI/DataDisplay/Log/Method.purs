module Blessed.UI.DataDisplay.Log.Method where


import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.JsApi (NodeId) as C
import Blessed.Internal.Core (method) as C


-- FIXME: Methods from Element


log :: forall m. String -> C.NodeId -> BlessedOp m
log what nodeId =
    C.method nodeId "log" [ C.arg CA.string what ]
