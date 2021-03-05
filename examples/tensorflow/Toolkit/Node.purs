module TensorFlow.Toolkit.Node where

import Prelude

import Data.Lens.Lens.Product (_1)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Noodle.Process (ProcessF(..)) as R
import Noodle.Render.Atom (class Atom) as R
import Noodle.Toolkit ((~<), (>~))
import Noodle.Toolkit (NodeDef(..), defineNode, noInlets, noOutlets, withInlets, withOutlets) as T
import TensorFlow.Toolkit.Channel (Channel(..))
import TensorFlow.Toolkit.Value (Value(..))


type NodeDef = T.NodeDef Value Channel


data Node
    = BangNode
    | NodeListNode
    | AddNode
    | LayerNode


nodesForTheList :: Array Node
nodesForTheList =
    [ BangNode
    , NodeListNode
    , AddNode
    , LayerNode
    ]


{- BANG NODE -}

bangNode :: NodeDef
bangNode =
    T.defineNode
        (T.withInlets
            ~< "bang" /\ Channel)
        (T.withOutlets
            >~ "bang" /\ Channel)
        $ R.Process
            $ \_ ->
                let
                    send "bang" = Just $ Bang
                    send _ = Nothing
                in pure send


{- LAYER NODE -}

layerNode :: NodeDef
layerNode =
    T.defineNode
        (T.withInlets
            ~< "bang" /\ Channel)
        (T.withOutlets
            >~ "layer" /\ Channel)
        $ R.Process
            $ \receive ->
                let
                    send "layer" = Just $ Layer 22
                    send _ = Nothing
                in pure send


{- ADD NODE -}

addNode :: NodeDef
addNode =
    T.defineNode
        (T.withInlets
            ~< "layer1" /\ Channel
            ~< "layer2" /\ Channel)
        (T.withOutlets
            >~ "layer" /\ Channel)
        $ R.Process
            $ \receive ->
                let
                    send "layer" = receive "layer1"
                    send _ = Nothing
                in pure send


instance nodeAtom :: R.Atom Node where
    labelOf = show
    uniqueIdOf = show
    debugInfoOf = show


instance showNode :: Show Node where
    show BangNode = "bang"
    show NodeListNode = "node list"
    show AddNode = "add"
    show LayerNode = "layer"

