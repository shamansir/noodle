module Blessed.UI.Checkbox.Method where

import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.JsApi (NodeId) as C
import Blessed.Internal.Core (method) as C


-- FIXME: Methods from Element


check :: forall m. C.NodeId -> BlessedOp m
check nodeId =
    C.method nodeId "check" [ ]


uncheck :: forall m. C.NodeId -> BlessedOp m
uncheck nodeId =
    C.method nodeId "uncheck" [ ]


toggle :: forall m. C.NodeId -> BlessedOp m
toggle nodeId =
    C.method nodeId "toggle" [ ]
