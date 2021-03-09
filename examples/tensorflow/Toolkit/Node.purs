module TensorFlow.Toolkit.Node where

import Prelude

import Data.Lens.Lens.Product (_1)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Noodle.Process (ProcessF(..), makeProcessST) as R
import Noodle.Render.Atom (class Atom) as R
import Noodle.Toolkit ((~<), (>~))
import Noodle.Toolkit (NodeDef(..), defineNode, noInlets, noOutlets, withInlets, withOutlets) as T
import TensorFlow.Toolkit.Channel (Channel(..))
import TensorFlow.Toolkit.Value (Value(..))
import TensorFlow.TfModel (TfModel(..), toCode)


type NodeDef = T.NodeDef Value Channel


data Node
    = BangNode
    | NodeListNode
    | AddNode
    | InputLayerNode
    | TfModelNode


nodesForTheList :: Array Node
nodesForTheList =
    [ BangNode
    , NodeListNode
    , AddNode
    , InputLayerNode
    , TfModelNode
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

inputLayerNode :: NodeDef
inputLayerNode =
    T.defineNode
        (T.withInlets
            ~< "bang" /\ Channel)
        (T.withOutlets
            >~ "layer" /\ Channel)
        $ R.Process
            $ \receive ->
                let
                    send "layer" = Just $ TF $ InputLayer
                    send _ = Nothing
                in pure send


{- TFMODEL NODE -}

tfModelNode :: NodeDef
tfModelNode =
    T.defineNode
        (T.withInlets
            ~< "model" /\ Channel)
        (T.withOutlets
            >~ "codestr" /\ Channel)
        $ R.Process
            $ \receive ->
                case receive "model" of
                    Just (TF model) ->
                        let
                            send "codestr" = Just $ Code $ toCode model
                            send _ = Nothing
                        in pure send
                    _ ->
                        pure $ pure Nothing


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
    show InputLayerNode = "input layer"
    show TfModelNode = "TfModel"

