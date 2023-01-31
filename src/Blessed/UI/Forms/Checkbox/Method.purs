module Blessed.UI.Forms.Checkbox.Method where

import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedSubj (Subject, Checkbox)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.Core (method) as C


-- FIXME: Methods from Element


check
    :: forall (subj :: Subject) (id :: Symbol) m
     . Respresents Checkbox subj id
    => NodeKey subj id -> BlessedOp m
check nodeId =
    C.method nodeId "check" [ ]


uncheck
    :: forall (subj :: Subject) (id :: Symbol) m
     . Respresents Checkbox subj id
    => NodeKey subj id -> BlessedOp m
uncheck nodeId =
    C.method nodeId "uncheck" [ ]


toggle
    :: forall (subj :: Subject) (id :: Symbol) m
     . Respresents Checkbox subj id
    => NodeKey subj id -> BlessedOp m
toggle nodeId =
    C.method nodeId "toggle" [ ]
