module Blessed.UI.Prompts.Prompt.Method where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedSubj (Subject, Prompt)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.Core (method) as C


input
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Prompt subj id
    => String -> String -> NodeKey subj id -> BlessedOp state m
input text value nodeId =
    C.method nodeId "input"
        [ C.arg CA.string text
        , C.arg CA.string value
        ]


{-
input' :: forall m. String -> String -> Callback -> C.NodeId -> BlessedOp state m
input' text value nodeId =
    C.method nodeId "input"
        [ C.arg CA.string text
        , C.arg CA.string value
        ]
-}