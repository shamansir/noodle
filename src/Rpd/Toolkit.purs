module Rpd.Toolkit
    ( Toolkit(..), ToolkitName(..)
    , NodeDefAlias(..), ChannelDefAlias(..)
    , InletAlias(..), OutletAlias(..)
    , NodeDef(..)
    , class Channels
    , default, adapt, accept--, show
    -- , nodes, inlets, outlets
    , inlets, outlets
    --, mkToolkitE
    , ToolkitRenderer, RendererAlias(..)
    , empty
    , emptyNode
    , (>~), (~<), andInlet, andOutlet
    , (>>~), (~<<), andInlets, andOutlets
    , withInlets, withOutlets, noInlets, noOutlets
    -- , class NodeRenderer, class ChannelRenderer
    --, renderNode, renderInlet, renderOutlet
    -- , RenderNode, RenderInlet, RenderOutlet
    ) where


import Prelude

import Effect (Effect)
import Data.Bifunctor (bimap)
import Data.Maybe (Maybe)
import Data.Map as Map
import Data.List (List, (:))
import Data.List as List
import Data.Tuple.Nested ((/\), type (/\))
import Data.Exists (Exists, mkExists)
import Data.Foldable (foldl, foldr)

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


-- FIXME: the name "Channel" is not right, it's rather Channels system... `Routing`, `ChannelDef`, `Transponder`?
-- class (Show c) <= Channel c d where
class Channels d c where
    default :: c -> d
    accept :: c -> d -> Boolean
    adapt :: c -> d -> d
    -- -- repr :: forall x. Show x => c -> d -> x
    -- show :: c -> d -> String


data NodeDef d c =
    NodeDef
        { process :: ProcessF d
        , inlets :: List (InletAlias /\ c)
        , outlets :: List (OutletAlias /\ c)
        }
        -- (Channels d c =>
        --     { process :: ProcessF d
        --     , inlets :: List (InletAlias /\ c)
        --     , outlets :: List (OutletAlias /\ c)
        --     })


data Toolkit d c n = Toolkit ToolkitName (n -> NodeDef d c)


type ToolkitRenderer d c n view action =
    -- FIXME: now any node, inlet and outlet are containing the corresponding `n` or 'c' instance
    --        so it's a duplication to use them as a separate parameters in these functions
    { renderNode :: n -> R.Node d n -> view
    -- , renderInlet :: ChannelDefAlias -> R.Inlet d -> c -> (msg -> Effect Unit) -> view
    , renderInlet :: c -> R.Inlet d c -> Maybe d -> view
    -- , renderOutlet :: ChannelDefAlias -> R.Outlet d -> c -> (msg -> Effect Unit) -> view
    , renderOutlet :: c -> R.Outlet d c -> Maybe d -> view
    }



empty :: forall d c n. String -> Toolkit d c n
empty tkName =
    Toolkit (ToolkitName tkName) $ const emptyNode


-- nodes
--     :: forall d c
--      . Array (String /\ NodeDef d c)
--     -> (NodeDefAlias /-> NodeDef d c)
-- nodes nodeArray =
--     nodeArray
--     # map (bimap NodeDefAlias identity)
--     # Map.fromFoldable


emptyNode :: forall d c. NodeDef d c
emptyNode =
    NodeDef
        { process : Withhold
        , inlets : List.Nil
        , outlets : List.Nil
        }


infixl 1 andInlet as ~<

infixl 1 andInlets as ~<<

infixl 1 andOutlet as >~

infixl 1 andOutlets as >>~


andInlet
    :: forall c
     . List (InletAlias /\ c)
    -> String /\ c
    -> List (InletAlias /\ c)
andInlet list (name /\ channel) =
    list `List.snoc` (InletAlias name /\ channel)


andInlets
    :: forall c
     . List (InletAlias /\ c)
    -> List (String /\ c)
    -> List (InletAlias /\ c)
andInlets list source =
    foldr
        (:)
        (source <#> (\(name /\ channel) -> InletAlias name /\ channel))
        list


andOutlet
    :: forall c
     . List (OutletAlias /\ c)
    -> String /\ c
    -> List (OutletAlias /\ c)
andOutlet list (name /\ channel) =
    list `List.snoc` (OutletAlias name /\ channel)


andOutlets
    :: forall c
     . List (OutletAlias /\ c)
    -> List (String /\ c)
    -> List (OutletAlias /\ c)
andOutlets list source =
    foldr
        (:)
        (source <#> (\(name /\ channel) -> OutletAlias name /\ channel))
        list


withInlets :: forall c. List (InletAlias /\ c)
withInlets = List.Nil


noInlets :: forall c. List (InletAlias /\ c)
noInlets = List.Nil


withOutlets :: forall c. List (OutletAlias /\ c)
withOutlets = List.Nil


noOutlets :: forall c. List (OutletAlias /\ c)
noOutlets = List.Nil


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
