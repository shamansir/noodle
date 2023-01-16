module Blessed.UI.Screen.Op where

import Prelude


import Blessed.Internal.Command (Command, call, arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.JsApi (NodeId) as C
import Blessed.Internal.BlessedOp (perform) as Op


render :: forall m. C.NodeId -> BlessedOp m
render nodeId =
    Op.perform nodeId $ C.call "render" [ ]