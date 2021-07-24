module Noodle.Patch where


import Noodle.Node (Node, Link)
import Noodle.Node as Node

import Prelude (($), (#), (<$>), (<*>), (<#>), pure)
import Effect (Effect)

-- import Data.Functor (lift)

import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested ((/\), type (/\))


type InletPath = String /\ String
type OutletPath = String /\ String


data Patch d =
    Patch
        (String /-> Node d)
        ((OutletPath /\ InletPath) /-> Link)


empty :: forall d. Patch d
empty = Patch Map.empty Map.empty


addNode :: forall d. String -> Node d -> Patch d -> Patch d
addNode name node (Patch nodes links) =
    Patch
        (nodes # Map.insert name node)
        links


nodes :: forall d. Patch d -> Array (String /\ Node d)
nodes (Patch nodes _) = nodes # Map.toUnfoldable


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


connect :: forall d. OutletPath -> InletPath -> Patch d -> Effect (Patch d)
connect (srcNodeName /\ outlet) (dstNodeName /\ inlet) patch =
    case (/\) <$> findNode srcNodeName patch <*> findNode dstNodeName patch of
        Just (srcNode /\ dstNode) ->
            Node.connect (srcNode /\ outlet) (dstNode /\ inlet)
                <#> \link ->
                    registerLink (srcNodeName /\ outlet) (dstNodeName /\ inlet) link patch
        Nothing -> pure patch