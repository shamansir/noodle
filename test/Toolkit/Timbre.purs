module Rpd.Test.Toolkit.Timbre
    ( toolkit
    , Data
    )
    where

import Prelude

import Data.List ((:))
import Data.List as List
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Rpd.Def as R
import Rpd.Toolkit as T


data WaveKind
    = Sin
    | Saw
    | Tri
    | Pulse
    | Fami


data Data
    = Value Number
    | Wave WaveKind
    | Sound


toolkit :: T.Toolkit Data
toolkit =
    { id : "timbre"
    , nodeDefs :
        T.defs
            [ ( "num" /\ numNode )
            ]
    , channelDefs : T.noDefs
    }


numNode :: R.NodeDef Data
numNode =
    { name : "num" -- TODO: add both `id`` and `name`` to nodes
    , inletDefs : List.Nil
    , outletDefs
        : numOutlet
        : List.Nil
    , process : R.FlowThrough
    }


numOutlet :: R.OutletDef Data
numOutlet =
    { label : "num"
    }
