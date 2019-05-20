module Rpd.Toolkit
    ( Toolkit
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


-- newtype NodeAlias = NodeAlias String
-- newtype ChannelAlias = ChannelAlias String
-- newtype InletAlias = InletAlias String
-- newtype OutletAlias = OutletAlias String


type Toolkit d c =
    Channel c d =>
        { name :: String
        , nodes :: String /->
            { name :: String
            , process :: ProcessF d
            , inlets :: List (String /\ c)
            , outlets :: List (String /\ c)
            }
        }


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
