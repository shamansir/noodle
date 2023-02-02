module Blessed.UI.Prompts.Loading.Method where


import Prelude

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedSubj (Subject, Loading)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.Core (method) as C



load
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Loading subj id
    => String -> NodeKey subj id -> BlessedOp state m
load text nodeId =
    C.method nodeId "load"
        [ C.arg CA.string text
        ]


stop
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Loading subj id
    => NodeKey subj id-> BlessedOp state m
stop nodeId =
    C.method nodeId "stop" [ ]
