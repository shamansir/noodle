module Noodle.Patch where


import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)

-- import Data.Functor (lift)

import Data.Set (Set)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Foldable (foldr)
import Data.Traversable (sequence, traverse)
import Data.Tuple.Nested ((/\), type (/\))


import Noodle.Node (Node, Link)
import Noodle.Node as Node
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit as Toolkit



type InletPath = String /\ String
type OutletPath = String /\ String


data Patch d =
    Patch
        (String /-> Node d)
        ((OutletPath /\ InletPath) /-> Link)


infixl 4 connect as <~>
infixl 4 disconnect as <~/~>
infixl 5 send' as +>
infixl 5 produce' as ++>


empty :: forall d. Patch d
empty = Patch Map.empty Map.empty


addNode :: forall d. String -> Node d -> Patch d -> Patch d
addNode name node (Patch nodes links) =
    Patch
        (nodes # Map.insert name node)
        links


addNodeFrom :: forall d. Toolkit d -> String /\ String -> Patch d -> Effect (Patch d)
addNodeFrom toolkit (nodeType /\ nodeId) patch =
    Toolkit.spawn nodeType toolkit
        <#> maybe patch (\node -> addNode nodeId node patch)


addNodeFrom' :: forall d. Toolkit d -> String -> Patch d -> Effect (String /\ Patch d)
addNodeFrom' toolkit nodeType patch =
    addNodeFrom toolkit (nodeType /\ nextNodeId) patch
        <#> ((/\) nextNodeId)
    where nextNodeId = addUniqueNodeId patch nodeType


addNodesFrom :: forall d. Toolkit d -> Array (String /\ String) -> Patch d -> Effect (Patch d)
addNodesFrom toolkit pairs patch =
    foldr (\pair patchEff -> patchEff >>= addNodeFrom toolkit pair) (pure patch) pairs


nodes :: forall d. Patch d -> Array (String /\ Node d)
nodes (Patch nodes _) = nodes # Map.toUnfoldable


links :: forall d. Patch d -> Set (OutletPath /\ InletPath)
links (Patch _ links) = links # Map.keys


findNode :: forall d. String -> Patch d -> Maybe (Node d)
findNode name (Patch nodes _) = nodes # Map.lookup name


nodesCount :: forall d. Patch d -> Int
nodesCount (Patch nodes _) = Map.size nodes


linksCount :: forall d. Patch d -> Int
linksCount (Patch _ links) = Map.size links


registerLink :: forall d. OutletPath -> InletPath -> Link -> Patch d -> Patch d
registerLink outletPath inletPath link (Patch nodes links) =
    Patch nodes
        $ Map.insert (outletPath /\ inletPath) link
        $ links


forgetLink :: forall d. OutletPath -> InletPath -> Patch d -> Patch d
forgetLink outletPath inletPath (Patch nodes links) =
    Patch nodes
        $ Map.delete (outletPath /\ inletPath)
        $ links


connect :: forall d. OutletPath -> InletPath -> Patch d -> Effect (Patch d)
connect (srcNodeName /\ outlet) (dstNodeName /\ inlet) patch =
    case (/\) <$> findNode srcNodeName patch <*> findNode dstNodeName patch of
        Just (srcNode /\ dstNode) ->
            Node.connect (srcNode /\ outlet) (dstNode /\ inlet)
                <#> \link ->
                    registerLink (srcNodeName /\ outlet) (dstNodeName /\ inlet) link patch
        Nothing -> pure patch


disconnect :: forall d. OutletPath -> InletPath -> Patch d -> Effect (Patch d)
disconnect outletPath inletPath patch@(Patch _ links) =
    case links # Map.lookup (outletPath /\ inletPath) of
        Just link -> do
            Node.disconnect link
            pure $ forgetLink outletPath inletPath patch
        Nothing ->
            pure patch


send :: forall d. (String /\ String) -> d -> Patch d -> Effect Unit
send (node /\ inlet) v patch =
    patch
        # findNode node
        <#> (flip Node.send (inlet /\ v))
        # fromMaybe (pure unit)


send' :: forall d. (String /\ String) -> d -> Patch d -> Effect (Patch d)
send' path v patch =
    send path v patch *> pure patch


produce :: forall d. (String /\ String) -> d -> Patch d -> Effect Unit
produce (node /\ outlet) v patch =
    patch
        # findNode node
        <#> (flip Node.produce (outlet /\ v))
        # fromMaybe (pure unit)


produce' :: forall d. (String /\ String) -> d -> Patch d -> Effect (Patch d)
produce' path v patch =
    produce path v patch *> pure patch


addUniqueNodeId :: forall d. Patch d -> String -> String
addUniqueNodeId patch nodeType =
    nodeType <> "-" <> (show $ nodesCount patch + 1)


-- TODO: `withNode`