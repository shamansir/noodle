module Blessed.UI.DataDisplay.Log.Method where


import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedSubj (Subject, Log)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.Core (method) as C


-- FIXME: Methods from Element


log
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Log subj id
    => String -> NodeKey subj id -> BlessedOp state m
log what nodeId =
    C.method nodeId "log" [ C.arg CA.string what ]
