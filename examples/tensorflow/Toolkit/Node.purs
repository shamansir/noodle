module TensorFlow.Toolkit.Node where

import Prelude

import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))

import Noodle.Process (ProcessF(..)) as R
import Noodle.Toolkit (NodeDef(..), noInlets, noOutlets, withInlets, withOutlets, defineNode) as T
import Noodle.Toolkit ((~<), (>~))
import Noodle.Render.Atom (class Atom) as R

import TensorFlow.Toolkit.Value (Value(..))
import TensorFlow.Toolkit.Channel (Channel(..))


type NodeDef = T.NodeDef Value Channel


data Node
    = NodeListNode
    | AddNode
    | LayerNode


nodesForTheList :: Array Node
nodesForTheList =
    [ NodeListNode
    , AddNode
    , LayerNode
    ]


{- LAYER NODE -}

layerNode :: NodeDef
layerNode =
    T.defineNode
        (T.withInlets
            ~< "one" /\ Channel)
        (T.noOutlets)
        $ R.Withhold


{- ADD NODE -}

addNode :: NodeDef
addNode =
    T.defineNode
        (T.withInlets
            ~< "one" /\ Channel)
        (T.noOutlets)
        $ R.Withhold


instance nodeAtom :: R.Atom Node where
    labelOf = show
    uniqueIdOf = show
    debugInfoOf = show


instance showNode :: Show Node where
    show NodeListNode = "node list"
    show AddNode = "add"
    show LayerNode = "layer"

