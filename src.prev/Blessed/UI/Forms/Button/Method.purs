module Blessed.UI.Forms.Button.Method where

import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedSubj (Subject, Button)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.Core (method) as C


-- FIXME: Methods from Element


press
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Button subj id
    => NodeKey subj id -> BlessedOp state m
press nodeId =
    C.method nodeId "press" [ ]
