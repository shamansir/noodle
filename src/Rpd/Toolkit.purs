module Rpd.Toolkit
    ( Toolkit, ToolkitName(..)
    , NodeDefAlias(..), ChannelDefAlias(..)
    , InletAlias(..), OutletAlias(..)
    , NodeDef
    ) where

import Prelude

import Data.String
import Data.Maybe
import Data.Map as Map
import Data.List
import Data.Tuple.Nested ((/\), type (/\))

import Rpd.Util (type (/->))
import Rpd.Process (ProcessF)
import Rpd.Channel (class Channel)


newtype ToolkitName = ToolkitName String
newtype NodeDefAlias = NodeDefAlias String
newtype ChannelDefAlias = ChannelDefAlias String
newtype InletAlias = InletAlias String
newtype OutletAlias = OutletAlias String


type NodeDef d =
    { process :: ProcessF d
    , inlets :: List (InletAlias /\ ChannelDefAlias)
    , outlets :: List (OutletAlias /\ ChannelDefAlias)
    }


type Toolkit c d =
    Channel c d =>
        { name :: ToolkitName
        , nodes :: NodeDefAlias /-> NodeDef d
        , channels :: ChannelDefAlias /-> c
        }


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
