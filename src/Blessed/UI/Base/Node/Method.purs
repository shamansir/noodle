module Blessed.UI.Base.Node.Method where


import Data.Maybe (Maybe)

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedSubj (Element, Subject)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.Core (method) as C


prepend
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => String -> NodeKey subj id -> BlessedOp state m
prepend node nodeId =
    C.method nodeId "prepend" [ C.arg CA.string node ]


append
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => String -> NodeKey subj id -> BlessedOp state m
append node nodeId =
    C.method nodeId "append" [ C.arg CA.string node ]


remove
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => String -> NodeKey subj id -> BlessedOp state m
remove node nodeId =
    C.method nodeId "remove" [ C.arg CA.string node ]


insert
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => String -> Int -> NodeKey subj id -> BlessedOp state m
insert node i nodeId =
    C.method nodeId "insert" [ C.arg CA.string node, C.arg CA.int i ]


insertBefore
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => String -> String -> NodeKey subj id -> BlessedOp state m
insertBefore node refNode nodeId =
    C.method nodeId "insertBefore" [ C.arg CA.string node, C.arg CA.string refNode ]


insertAfter
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => String -> String -> NodeKey subj id -> BlessedOp state m
insertAfter node refNode nodeId =
    C.method nodeId "insertAfter" [ C.arg CA.string node, C.arg CA.string refNode ]


detach
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    =>  NodeKey subj id -> BlessedOp state m
detach nodeId =
    C.method nodeId "detach" [ ]


emitDescandants
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    =>  NodeKey subj id -> BlessedOp state m
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