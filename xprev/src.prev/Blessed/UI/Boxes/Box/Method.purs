module Blessed.UI.Boxes.Box.Method where

import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedSubj (Subject, Box)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.Core (method) as C


-- FIXME: Methods from Element


setContent
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Box subj id
    => String -> NodeKey subj id -> BlessedOp state m
setContent value nodeId =
    C.method nodeId "setContent"
        [ C.arg CA.string value
        ]


setLine
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Box subj id
    => Int -> String -> NodeKey subj id -> BlessedOp state m
setLine n value nodeId =
    C.method nodeId "setLine"
        [ C.arg CA.int n
        , C.arg CA.string value
        ]


insertLine
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Box subj id
    => Int -> String -> NodeKey subj id -> BlessedOp state m
insertLine n value nodeId =
    C.method nodeId "insertLine"
        [ C.arg CA.int n
        , C.arg CA.string value
        ]


focus
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Box subj id
    => NodeKey subj id -> BlessedOp state m
focus nodeId =
    C.method nodeId "focus" []


{-
setContent value:String
setLine n:Int value:String
insertLine n:Int value:String
focus
-}