module Blessed.UI.Base.Node.Method where


import Data.Maybe (Maybe)

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Command (arg, narg, node) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedSubj (Element, Subject)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.Core (method, nmethod, Blessed) as C

    -- FIXME: handlers returned from `encode'`!!
    -- FIXME: ensure to append and encode properly in BlessedOp.js


-- refHelper fn nodeId node =
--     BlessedOp.getStateRef >>=
--         \stateRef -> fn stateRef [ C.arg (Codec.encode' (Just $ NodeKey.rawify nodeId) stateRef node) node ]


prepend
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => C.Blessed state -> NodeKey subj id -> BlessedOp state m
prepend node nodeId =
    C.nmethod nodeId "prepend" [ C.node node ]


append
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => C.Blessed state -> NodeKey subj id -> BlessedOp state m
append node nodeId =
    C.nmethod nodeId "append" [ C.node node ]


remove
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => C.Blessed state -> NodeKey subj id -> BlessedOp state m
remove node nodeId =
    C.nmethod nodeId "remove" [ C.node node ]


insert
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => C.Blessed state -> Int -> NodeKey subj id -> BlessedOp state m
insert node i nodeId =
    C.nmethod nodeId "insert" [ C.node node, C.narg CA.int i ]


insertBefore
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => C.Blessed state -> C.Blessed state -> NodeKey subj id -> BlessedOp state m
insertBefore node refNode nodeId =
    C.nmethod nodeId "insertBefore" [ C.node node, C.node refNode ]


insertAfter
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => C.Blessed state -> C.Blessed state -> NodeKey subj id -> BlessedOp state m
insertAfter node refNode nodeId =
    C.nmethod nodeId "insertAfter" [ C.node node, C.node refNode ]


detach
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
detach nodeId =
    C.method nodeId "detach" [ ]


emitDescandants
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
emitDescandants nodeId =
    C.method nodeId "emitDescandants" [ ]


get
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => String -> Maybe Json -> NodeKey subj id -> BlessedOp state m -- FIXME
get name value nodeId =
    C.method nodeId "get" [ C.arg CA.string name, C.arg (CAC.maybe CA.json) value ]


set
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => String -> Json -> NodeKey subj id -> BlessedOp state m
set name value nodeId =
    C.method nodeId "set" [ C.arg CA.string name, C.arg CA.json value ]


{-
prepend node:String
append node:String
remove node:String
insert node:String i:Int
insertBefore node:String refNode:String
insertAfter node:String refNode:String
detach
emitDescandants
get name:String value:MaybeJson
set name:String value:Json
-}