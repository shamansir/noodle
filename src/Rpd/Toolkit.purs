module Rpd.Toolkit
    ( Toolkit, ToolkitName(..)
    , NodeDefAlias(..), ChannelDefAlias(..)
    , InletAlias(..), OutletAlias(..)
    , NodeDef
    , class Channel
    , default, adapt, accept--, show
    ) where

import Prelude

import Data.String
import Data.Maybe
import Data.Map as Map
import Data.List
import Data.Tuple.Nested ((/\), type (/\))

import Rpd.Util (type (/->))
import Rpd.Process (ProcessF)


newtype ToolkitName = ToolkitName String
newtype NodeDefAlias = NodeDefAlias String
newtype ChannelDefAlias = ChannelDefAlias String
newtype InletAlias = InletAlias String
newtype OutletAlias = OutletAlias String


-- FIXME: the name "Channel" is not right, it's rather Channels system... `ChannelDef`?
class (Show c) <= Channel c d where
    default :: c -> d
    accept :: c -> d -> Boolean
    adapt :: c -> d -> d
    -- -- repr :: forall x. Show x => c -> d -> x
    -- show :: c -> d -> String


type NodeDef c d = -- TODO: Should also require `Channel c d` restriction?
    { process :: ProcessF d
    , inlets :: List (InletAlias /\ c)
    , outlets :: List (OutletAlias /\ c)
    }


type Toolkit c d =
    Channel c d =>
        { name :: ToolkitName
        , nodes :: NodeDefAlias /-> NodeDef c d
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
