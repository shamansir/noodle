module Blessed.UI.DataDisplay.Table.Method where


import Data.Maybe (Maybe)

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedSubj (Subject, Table)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.Core (method) as C


setRows
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Table subj id
    => Array (Array String) -> NodeKey subj id -> BlessedOp state m
setRows data_ nodeId =
    C.method nodeId "setRows" [ C.arg (CA.array (CA.array CA.string)) data_ ]
