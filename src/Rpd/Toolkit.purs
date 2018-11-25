module Rpd.Toolkit
    ( Toolkit
    , defs, noDefs, singleDef
    , findNodeDef
    ) where

import Prelude

import Data.String
import Data.Maybe
import Data.Map as Map
import Data.Tuple.Nested ((/\))

import Rpd.Util (type (/->))
import Rpd.Def as D

type Toolkit d =
    { id :: String
    , nodeDefs :: String /-> D.NodeDef d
    , channelDefs :: String /-> D.ChannelDef d
    }


noDefs = Map.empty
defs = Map.fromFoldable
singleDef label def = defs [ ( label /\ def ) ]


findNodeDef :: forall d. String -> Toolkit d -> Maybe (D.NodeDef d)
findNodeDef nodePath toolkit =
    case split (Pattern "/") nodePath of
        [ toolkitId, nodeName ] ->
            if (toolkitId == toolkit.id) then
                Map.lookup nodeName toolkit.nodeDefs
            else Nothing
        _ -> Nothing
