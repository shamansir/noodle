module Rpd.Toolkit
    ( Toolkit, ToolkitName(..), Toolkits(..)--, ToolkitE
    , NodeDefAlias(..), ChannelDefAlias(..)
    , InletAlias(..), OutletAlias(..)
    , NodeDef(..)
    , class Channels
    , default, adapt, accept--, show
    , nodes, inlets, outlets
    --, mkToolkitE
    , ToolkitRenderer, ToolkitsRenderer, RendererAlias(..)
    -- , class NodeRenderer, class ChannelRenderer
    --, renderNode, renderInlet, renderOutlet
    -- , RenderNode, RenderInlet, RenderOutlet
    ) where


import Prelude

import Effect (Effect)
import Data.Bifunctor (bimap)
import Data.Maybe (Maybe)
import Data.Map as Map
import Data.List (List)
import Data.List as List
import Data.Tuple.Nested (type (/\))
import Data.Exists (Exists, mkExists)

import Rpd.Util (type (/->))
import Rpd.Process (ProcessF(..))
import Rpd.Network (Node, Inlet, Outlet) as R
-- import Rpd.Command (Command) as R


newtype ToolkitName = ToolkitName String
newtype NodeDefAlias = NodeDefAlias String
newtype ChannelDefAlias = ChannelDefAlias String
newtype InletAlias = InletAlias String
newtype OutletAlias = OutletAlias String
newtype RendererAlias = RendererAlias String


instance showToolkitName :: Show ToolkitName where
    show (ToolkitName name) = "[" <> name <> "]"
instance showNodeDefAlias :: Show NodeDefAlias where
    show (NodeDefAlias alias) = "[" <> alias <> "]"
instance showChannelDefAlias :: Show ChannelDefAlias where
    show (ChannelDefAlias alias) = "[" <> alias <> "]"


-- FIXME: `msg` should always be equal to `Rpd.Command`
-- type RenderNode d msg view = forall msg. R.Node d -> (msg -> Effect Unit) -> view
-- type RenderInlet c d msg view = Channels d c => (R.Inlet d -> c -> (msg -> Effect Unit) -> view)
-- type RenderOutlet c d msg view = Channels d c => (R.Outlet d -> c -> (msg -> Effect Unit) -> view)


-- class NodeRenderer msg view d where
--     renderNode :: NodeDefAlias -> R.Node d -> (msg -> Effect Unit) -> view


-- class Channels d c <= ChannelRenderer msg view d c where
--     renderInlet :: ChannelDefAlias -> R.Inlet d -> c -> (msg -> Effect Unit) -> view
--     renderOutlet :: ChannelDefAlias -> R.Outlet d -> c -> (msg -> Effect Unit) -> view

-- data Renderer d c msg view = Renderer
--     { node :: NodeDefAlias /-> RenderNode d msg view
--     , inlet :: ChannelDefAlias /-> RenderInlet c d msg view
--     , outlet :: ChannelDefAlias /-> RenderOutlet c d msg view
--     }


-- FIXME: the name "Channel" is not right, it's rather Channels system... `ChannelDef`, `Transponder`?
-- class (Show c) <= Channel c d where
class Channels d c where
    default :: c -> d
    accept :: c -> d -> Boolean
    adapt :: c -> d -> d
    -- -- repr :: forall x. Show x => c -> d -> x
    -- show :: c -> d -> String


data NodeDef d c =
    NodeDef
        (Channels d c =>
            { process :: ProcessF d
            , inlets :: List (InletAlias /\ c)
            , outlets :: List (OutletAlias /\ c)
            })


-- class Channels d c <= Toolkit d c where
--     getNode :: NodeDefAlias -> NodeDef d c


type Toolkit d c = NodeDefAlias -> Maybe (NodeDef d c)

-- type ToolkitE d = Exists (Toolkit d)

-- mkToolkitE :: forall d c. Channels d c => Toolkit d c -> ToolkitE d
-- mkToolkitE = mkExists

-- type Toolkits d = forall toolkit. ToolkitName -> (forall c. Toolkit d c => toolkit)
type Toolkits d = ToolkitName -> Maybe (forall c. NodeDefAlias -> Maybe (NodeDef d c))

type ToolkitRenderer d c view msg =
    { renderNode :: NodeDefAlias -> R.Node d -> (msg -> Effect Unit) -> view
    , renderInlet :: ChannelDefAlias -> R.Inlet d -> c -> (msg -> Effect Unit) -> view
    , renderOutlet :: ChannelDefAlias -> R.Inlet d -> c -> (msg -> Effect Unit) -> view
    }

type ToolkitsRenderer d view msg =
    ToolkitName ->
        Maybe (forall c. ToolkitRenderer d c view msg)


-- data Toolkits d = Toolkits (ToolkitName /-> (forall c. Toolkit c d))


nodes
    :: forall d c
     . Array (String /\ NodeDef d c)
    -> (NodeDefAlias /-> NodeDef d c)
nodes nodeArray =
    nodeArray
    # map (bimap NodeDefAlias identity)
    # Map.fromFoldable


inlets
    :: forall c
    -- . (Channel c d)
     . Array (String /\ c)
    -> List (InletAlias /\ c)
inlets inletArray =
    inletArray
    # map (bimap InletAlias identity)
    # List.fromFoldable


outlets
    :: forall c
    -- . (Channel c d)
     . Array (String /\ c)
    -> List (OutletAlias /\ c)
outlets outletArray =
    outletArray
    # map (bimap OutletAlias identity)
    # List.fromFoldable


-- TODO: Toolkit w/o a Channel restriction should also be an option
-- type PlainToolkit d =
--     { name :: ToolkitName
--     , nodes :: NodeDefAlias /-> ...
--     }


derive instance eqChannelDefAlias :: Eq ChannelDefAlias
derive instance ordChannelDefAlias :: Ord ChannelDefAlias

derive instance eqNodeDefAlias :: Eq NodeDefAlias
derive instance ordNodeDefAlias :: Ord NodeDefAlias

-- noDefs = Map.empty
-- defs = Map.fromFoldable
-- singleDef label def = defs [ ( label /\ def ) ]


-- findNodeDef :: forall d. String -> Toolkit d -> Maybe (D.NodeDef d)
-- findNodeDef nodePath toolkit =
--     case split (Pattern "/") nodePath of
--         [ toolkitId, nodeName ] ->
--             if (toolkitId == toolkit.id) then
--                 Map.lookup nodeName toolkit.nodeDefs
--             else Nothing
--         _ -> Nothing
