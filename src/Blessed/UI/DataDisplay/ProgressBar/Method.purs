module Blessed.UI.DataDisplay.ProgressBar.Method where

import Prelude

import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedSubj (Subject, ProgressBar)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.Core (method) as C


-- FIXME: Methods from Element

progress
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents ProgressBar subj id
    => Int -> NodeKey subj id -> BlessedOp state m
progress amount nodeId =
    C.method nodeId "progress"
        [ C.arg CA.int amount ]


reset
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents ProgressBar subj id
    => NodeKey subj id -> BlessedOp state m
reset nodeId =
    C.method nodeId "reset" [ ]
