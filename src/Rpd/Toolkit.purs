module Rpd.Toolkit
    ( Toolkit(..), ToolkitName(..)
    , NodeDefAlias(..), ChannelDefAlias(..)
    , InletAlias(..), OutletAlias(..)
    , NodeDef(..)
    , class Channels
    , default, adapt, accept--, show
    , nodes, inlets, outlets
    ) where

import Prelude

import Data.Bifunctor (bimap)
import Data.Map as Map
import Data.List (List)
import Data.List as List
import Data.Tuple.Nested (type (/\))

import Rpd.Util (type (/->))
import Rpd.Process (ProcessF)


newtype ToolkitName = ToolkitName String
newtype NodeDefAlias = NodeDefAlias String
newtype ChannelDefAlias = ChannelDefAlias String
newtype InletAlias = InletAlias String
newtype OutletAlias = OutletAlias String


instance showToolkitName :: Show ToolkitName where
    show (ToolkitName name) = "[" <> name <> "]"
instance showNodeDefAlias :: Show NodeDefAlias where
    show (NodeDefAlias alias) = "[" <> alias <> "]"
instance showChannelDefAlias :: Show ChannelDefAlias where
    show (ChannelDefAlias alias) = "[" <> alias <> "]"


-- FIXME: the name "Channel" is not right, it's rather Channels system... `ChannelDef`, `Transponder`?
-- class (Show c) <= Channel c d where
class Channels c d where
    default :: c -> d
    accept :: c -> d -> Boolean
    adapt :: c -> d -> d
    -- -- repr :: forall x. Show x => c -> d -> x
    -- show :: c -> d -> String


data NodeDef c d =
    NodeDef
        (Channels c d =>
            { process :: ProcessF d
            , inlets :: List (InletAlias /\ c)
            , outlets :: List (OutletAlias /\ c)
            })

-- data NodeDef d =
--     NodeDef
--         (forall c. Channel c d =>
--             { process :: ProcessF d
--             , inlets :: List (InletAlias /\ c)
--             , outlets :: List (OutletAlias /\ c)
--             })


data Toolkit c d =
    Toolkit
        (Channels c d =>
            { name :: ToolkitName
            , nodes :: NodeDefAlias /-> NodeDef c d
            })


nodes
    :: forall c d
     . Array (String /\ NodeDef c d)
    -> (NodeDefAlias /-> NodeDef c d)
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
