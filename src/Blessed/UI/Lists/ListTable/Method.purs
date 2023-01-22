module Blessed.UI.Lists.ListTable.Method where


import Data.Maybe (Maybe)

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.JsApi (NodeId) as C
import Blessed.Internal.Core (method) as C


setRows :: forall m. Array (Array String) -> C.NodeId -> BlessedOp m
setRows data_ nodeId =
    C.method nodeId "prepend" [ C.arg (CA.array (CA.array CA.string)) data_ ]
