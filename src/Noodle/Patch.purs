module Noodle.Patch where


import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

-- import Data.Functor (lift)

import Data.Set (Set)
import Data.List (List)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Foldable (foldr, foldM)
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst, snd, curry, uncurry) as Tuple
import Data.Tuple.Nested ((/\), type (/\))


import Noodle.Node (Node, Link)
import Noodle.Node as Node
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit as Toolkit
-- import Noodle.PatchM (PatchM)


type Id = String


type InletPath = Node.Id /\ Node.InletId
type OutletPath = Node.Id /\ Node.OutletId


data Patch node_state m d =
    Patch
        (Node.Id /-> Node node_state m d)
        ((OutletPath /\ InletPath) /-> Link)


infixl 4 connect as <~>
infixl 4 disconnect as <~/~>
infixl 5 send' as +>
infixl 5 produce' as ++>


empty :: forall node_state m d. Patch node_state m d
empty = Patch Map.empty Map.empty


addNode :: forall node_state m d. Node.Id -> Node node_state m d -> Patch node_state m d -> Patch node_state m d
addNode name node (Patch nodes links) =
    Patch
        (nodes # Map.insert name node)
        links


addNodeFrom :: forall node_state d. Toolkit node_state Aff d -> node_state -> Node.Family /\ Node.Id -> Patch node_state Aff d -> Effect (Patch node_state Aff d)
addNodeFrom toolkit state (nodeFamily /\ nodeId) patch =
    Toolkit.spawn nodeFamily state toolkit
        <#> maybe patch (\node -> addNode nodeId node patch)


addNodeFrom' :: forall node_state d. Toolkit node_state Aff d -> node_state -> Node.Family -> Patch node_state Aff d -> Effect (Node.Id /\ Patch node_state Aff d)
addNodeFrom' toolkit state nodeFamily patch =
    addNodeFrom toolkit state (nodeFamily /\ nextNodeId) patch
        <#> ((/\) nextNodeId)
    where nextNodeId = addUniqueNodeId patch nodeFamily


addNodesFrom :: forall node_state d. Toolkit node_state Aff d -> node_state -> Array (Node.Family /\ Node.Id) -> Patch node_state Aff d -> Effect (Patch node_state Aff d)
addNodesFrom toolkit state pairs patch =
    foldr (\pair patchEff -> patchEff >>= addNodeFrom toolkit state pair) (pure patch) pairs


nodes :: forall node_state m d. Patch node_state m d -> Array (Node.Id /\ Node node_state m d)
nodes (Patch nodes _) = nodes # Map.toUnfoldable--Unordered


links :: forall node_state m d. Patch node_state m d -> Set (OutletPath /\ InletPath)
links (Patch _ links) = links # Map.keys


findNode :: forall node_state m d. Node.Id -> Patch node_state m d -> Maybe (Node node_state m d)
findNode name (Patch nodes _) = nodes # Map.lookup name


nodesCount :: forall node_state m d. Patch node_state m d -> Int
nodesCount (Patch nodes _) = Map.size nodes


linksCount :: forall node_state m d. Patch node_state m d -> Int
linksCount (Patch _ links) = Map.size links


registerLink :: forall node_state m d. OutletPath -> InletPath -> Link -> Patch node_state m d -> Patch node_state m d
registerLink outletPath inletPath link (Patch nodes links) =
    Patch nodes
        $ Map.insert (outletPath /\ inletPath) link
        $ links


forgetLink :: forall node_state m d. OutletPath -> InletPath -> Patch node_state m d -> Patch node_state m d
forgetLink outletPath inletPath (Patch nodes links) =
    Patch nodes
        $ Map.delete (outletPath /\ inletPath)
        $ links


forgetNode :: forall node_state m d. Node.Id -> Patch node_state m d -> Patch node_state m d
forgetNode nodeId (Patch nodes links) =
    Patch
        (Map.delete nodeId nodes)
        links


connect :: forall node_state m d. OutletPath -> InletPath -> Patch node_state m d -> Effect (Patch node_state m d)
connect (srcNodeName /\ outlet) (dstNodeName /\ inlet) patch =
    case (/\) <$> findNode srcNodeName patch <*> findNode dstNodeName patch of
        Just (srcNode /\ dstNode) ->
            Node.connect (srcNode /\ outlet) (dstNode /\ inlet)
                <#> \link ->
                    registerLink (srcNodeName /\ outlet) (dstNodeName /\ inlet) link patch
        Nothing -> pure patch


disconnect :: forall node_state m d. OutletPath -> InletPath -> Patch node_state m d -> Effect (Patch node_state m d)
disconnect outletPath inletPath patch@(Patch _ links) =
    case links # Map.lookup (outletPath /\ inletPath) of
        Just link -> do
            Node.disconnect link
            pure $ forgetLink outletPath inletPath patch
        Nothing ->
            pure patch


removeNode :: forall node_state m d. Node.Id -> Patch node_state m d -> Effect (Patch node_state m d)
removeNode nodeId patch@(Patch nodes _) =
    let linksWithNode = patch # linksToFromNode nodeId
    in do
        noMoreLinksPatch <- foldM (flip $ Tuple.uncurry disconnect) patch (Tuple.fst <$> linksWithNode)
        pure $ forgetNode nodeId noMoreLinksPatch


linksStartingFrom :: forall node_state m d. OutletPath -> Patch node_state m d -> Array (InletPath /\ Link)
linksStartingFrom outletPath (Patch _ links) =
    links
        # Map.toUnfoldable
        # Array.mapMaybe
            (\((outletPath' /\ inletPath) /\ link) ->
                if outletPath' == outletPath
                then Just $ inletPath /\ link
                else Nothing
            )


linksLeadingTo :: forall node_state m d. InletPath -> Patch node_state m d -> Array (OutletPath /\ Link)
linksLeadingTo inletPath (Patch _ links) =
    links
        # Map.toUnfoldable
        # Array.mapMaybe
            (\((outletPath /\ inletPath') /\ link) ->
                if inletPath' == inletPath
                then Just $ outletPath /\ link
                else Nothing
            )


linksToNode :: forall node_state m d. Node.Id -> Patch node_state m d -> Array ((OutletPath /\ InletPath) /\ Link)
linksToNode nodeId (Patch _ links) =
    links
        # Map.toUnfoldable
        # Array.mapMaybe
            (\(linkPath@(_ /\ (nodeId' /\ _)) /\ link) ->
                if nodeId' == nodeId
                then Just $ linkPath /\ link
                else Nothing
            )


linksFromNode :: forall node_state m d. Node.Id -> Patch node_state m d -> Array ((OutletPath /\ InletPath) /\ Link)
linksFromNode nodeId (Patch _ links) =
    links
        # Map.toUnfoldable
        # Array.mapMaybe
            (\(linkPath@((nodeId' /\ _) /\ _) /\ link) ->
                if nodeId' == nodeId
                then Just $ linkPath /\ link
                else Nothing
            )


linksToFromNode :: forall node_state m d. Node.Id -> Patch node_state m d -> Array ((OutletPath /\ InletPath) /\ Link)
linksToFromNode node patch =
    Array.nubByEq samePath $ linksToNode node patch <> linksFromNode node patch
    where samePath (pathA /\ _) (pathB /\ _) = pathA == pathB


linksCountAtNode :: forall node_state m d. Node.Id -> Patch node_state m d -> Node.LinksCount
linksCountAtNode node patch =
    let
        linksData = Tuple.fst <$> linksToFromNode node patch
        addOne (Just n) = Just $ n + 1
        addOne Nothing = Just 1
        addInletInfo (_ /\ inletPath) map =
            case inletPath of
                node' /\ inlet -> if node == node' then Map.alter addOne inlet map else map
        addOutletInfo (outletPath /\ _) map =
            case outletPath of
                node' /\ outlet -> if node == node' then Map.alter addOne outlet map else map
    in
    (foldr addInletInfo Map.empty linksData)
    /\
    (foldr addOutletInfo Map.empty linksData)


send :: forall node_state m d. (Node.Id /\ Node.InletId) -> d -> Patch node_state m d -> Effect Unit
send (node /\ inlet) v patch =
    patch
        # findNode node
        <#> (flip Node.send (inlet /\ v))
        # fromMaybe (pure unit)


send' :: forall node_state m d. (Node.Id /\ Node.InletId) -> d -> Patch node_state m d -> Effect (Patch node_state m d)
send' path v patch =
    send path v patch *> pure patch


produce :: forall node_state m d. (Node.Id /\ Node.OutletId) -> d -> Patch node_state m d -> Effect Unit
produce (node /\ outlet) v patch =
    patch
        # findNode node
        <#> (flip Node.produce (outlet /\ v))
        # fromMaybe (pure unit)


produce' :: forall node_state m d. (Node.Id /\ Node.OutletId) -> d -> Patch node_state m d -> Effect (Patch node_state m d)
produce' path v patch =
    produce path v patch *> pure patch


addUniqueNodeId :: forall node_state m d. Patch node_state m d -> Node.Family -> Node.Id
addUniqueNodeId patch nodeFamily =
    nodeFamily <> "-" <> (show $ nodesCount patch + 1)


-- TODO: `withNode`