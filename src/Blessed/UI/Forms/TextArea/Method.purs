module Blessed.UI.Forms.TextArea.Method where


import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedSubj (Subject, TextArea)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.Core (method) as C

{- TODO -}


{-
submit
cancel
readInput cb:Blessed
readEditor cb:Blessed
getValue -> String
clearValue
setValue value:String
-}


sumbit
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents TextArea subj id
    => NodeKey subj id -> BlessedOp state m
sumbit nodeId =
    C.method nodeId "sumbit" [ ]


cancel
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents TextArea subj id
    => NodeKey subj id -> BlessedOp state m
cancel nodeId =
    C.method nodeId "cancel" [ ]


clearValue
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents TextArea subj id
    => NodeKey subj id -> BlessedOp state m
clearValue nodeId =
    C.method nodeId "clearValue" [ ]


setValue
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents TextArea subj id
    => String -> NodeKey subj id -> BlessedOp state m
setValue value nodeId =
    C.method nodeId "setValue"
        [ C.arg CA.string value
        ]