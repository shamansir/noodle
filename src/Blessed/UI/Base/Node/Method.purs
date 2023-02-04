module Blessed.UI.Base.Node.Method where


import Prelude ((>>=), ($))

import Data.Maybe (Maybe(..))

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Tuple (fst) as Tuple
import Data.Tuple.Nested ((/\))

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedOp (getStateRef) as BlessedOp
import Blessed.Internal.BlessedSubj (Element, Subject)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.NodeKey (rawify) as NodeKey
import Blessed.Internal.Core (method, Blessed) as C
import Blessed.Internal.Codec as Codec
import Blessed.Internal.Emitter (CoreEvent) as C


nodeAsArg stateRef parentNodeKey node =
    C.arg (Codec.nodeCodec) (Tuple.fst $ Codec.encode' stateRef (Just $ NodeKey.rawify parentNodeKey) node) -- FIXME: handlers returned from `encoe'`!!


-- refHelper fn nodeId node =
--     BlessedOp.getStateRef >>=
--         \stateRef -> fn stateRef [ C.arg (Codec.encode' (Just $ NodeKey.rawify nodeId) stateRef node) node ]


prepend
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => C.Blessed state C.CoreEvent -> NodeKey subj id -> BlessedOp state m
prepend node nodeId =
    BlessedOp.getStateRef >>=
        \stateRef -> C.method nodeId "prepend" [ nodeAsArg stateRef nodeId node ]


append
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => C.Blessed state C.CoreEvent -> NodeKey subj id -> BlessedOp state m
append node nodeId =
    BlessedOp.getStateRef >>=
        \stateRef -> C.method nodeId "append" [ nodeAsArg stateRef nodeId node ]


remove
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => C.Blessed state C.CoreEvent -> NodeKey subj id -> BlessedOp state m
remove node nodeId =
    BlessedOp.getStateRef >>=
        \stateRef -> C.method nodeId "remove" [ nodeAsArg stateRef nodeId node ]


insert
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => C.Blessed state C.CoreEvent -> Int -> NodeKey subj id -> BlessedOp state m
insert node i nodeId =
    BlessedOp.getStateRef >>=
        \stateRef -> C.method nodeId "insert" [ nodeAsArg stateRef nodeId node, C.arg CA.int i ]


insertBefore
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => C.Blessed state C.CoreEvent -> C.Blessed state C.CoreEvent -> NodeKey subj id -> BlessedOp state m
insertBefore node refNode nodeId =
    BlessedOp.getStateRef >>=
        \stateRef -> C.method nodeId "insertBefore" [ nodeAsArg stateRef nodeId node, nodeAsArg stateRef nodeId refNode ]


insertAfter
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => C.Blessed state C.CoreEvent -> C.Blessed state C.CoreEvent -> NodeKey subj id -> BlessedOp state m
insertAfter node refNode nodeId =
    BlessedOp.getStateRef >>=
        \stateRef -> C.method nodeId "insertAfter" [ nodeAsArg stateRef nodeId node, nodeAsArg stateRef nodeId refNode ]


detach
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    =>  NodeKey subj id -> BlessedOp state m
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