module Blessed.UI.Forms.Button.Method where

import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.JsApi (NodeId) as C
import Blessed.Internal.Core (method) as C


-- FIXME: Methods from Element


press :: forall m. C.NodeId -> BlessedOp m
press nodeId =
    C.method nodeId "press" [ ]
