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
import Rpd.Toolkit (Toolkit(..)) as R


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


mapFrom = Map.fromFoldable


toolkit :: R.Toolkit Data
toolkit =
    { id : "timbre"
    , nodes :
        mapFrom
            [ ( "num" /\ numNode )
            ]
    , channels : Map.empty
    }


numNode :: R.NodeDef Data
numNode =
    { name : "timbre"
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
