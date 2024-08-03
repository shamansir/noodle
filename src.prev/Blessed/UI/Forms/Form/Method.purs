module Blessed.UI.Forms.Form.Method where

import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedSubj (Subject, Form)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.Core (method) as C


focusNext
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Form subj id
    => NodeKey subj id -> BlessedOp state m
focusNext nodeId =
    C.method nodeId "focusNext" [ ]


focusPrevious
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Form subj id
    => NodeKey subj id -> BlessedOp state m
focusPrevious nodeId =
    C.method nodeId "focusPrevious" [ ]


sumbit
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Form subj id
    => NodeKey subj id -> BlessedOp state m
sumbit nodeId =
    C.method nodeId "sumbit" [ ]


cancel
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Form subj id
    => NodeKey subj id -> BlessedOp state m
cancel nodeId =
    C.method nodeId "cancel" [ ]


reset
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Form subj id
    => NodeKey subj id -> BlessedOp state m
reset nodeId =
    C.method nodeId "reset" [ ]


{-
focusNext
focusPrevious
submit
cancel
reset
-}
