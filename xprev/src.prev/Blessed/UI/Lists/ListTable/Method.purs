module Blessed.UI.Lists.ListTable.Method where


import Data.Maybe (Maybe)

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedSubj (Subject, ListTable)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.Core (method) as C



{- TODO -}

setRows
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents ListTable subj id
    => Array (Array String) -> NodeKey subj id -> BlessedOp state m
setRows data_ nodeId =
    C.method nodeId "setRows" [ C.arg (CA.array (CA.array CA.string)) data_ ]
