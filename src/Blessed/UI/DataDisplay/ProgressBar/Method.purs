module Blessed.UI.DataDisplay.ProgressBar.Method where

import Prelude

import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.JsApi (NodeId) as C
import Blessed.Internal.Core (method) as C


-- FIXME: Methods from Element

progress :: forall m. Int -> C.NodeId -> BlessedOp m
progress amount nodeId =
    C.method nodeId "progress"
        [ C.arg CA.int amount ]


reset :: forall m. C.NodeId -> BlessedOp m
reset nodeId =
    C.method nodeId "reset" [ ]
