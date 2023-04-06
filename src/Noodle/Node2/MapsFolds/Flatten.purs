module Noodle.Node2.MapsFolds.Flatten where

import Prelude

import Data.UniqueHash (UniqueHash)
-- import Data.Array ()
import Data.Map (Map)
import Data.Map (toUnfoldable) as Map
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (lmap)

import Noodle.Id (FamilyR, InputR, NodeId, NodeIdR, OutputR, reflect', splitR, reflectNodeIdR)


type NodeLineRec f repr repr_is repr_os =
    NodeId f /\ repr /\ Record repr_is /\ Record repr_os

type NodeLineMap repr =
    NodeIdR /\ repr /\ Map InputR repr /\ Map OutputR repr


flatten :: forall repr. NodeLineMap repr -> (FamilyR /\ UniqueHash) /\ repr /\ Array (InputR /\ repr) /\ Array (OutputR /\ repr)
flatten (nodeId /\ repr /\ inputsMap /\ outputsMap) = splitR nodeId /\ repr /\ Map.toUnfoldable inputsMap /\ Map.toUnfoldable outputsMap


flatten' :: forall repr. NodeLineMap repr -> (String /\ String) /\ repr /\ Array (String /\ repr) /\ Array (String /\ repr)
flatten' (nodeId /\ repr /\ inputsMap /\ outputsMap) = reflectNodeIdR nodeId /\ repr /\ (lmap reflect' <$> Map.toUnfoldable inputsMap) /\ (lmap reflect' <$> Map.toUnfoldable outputsMap)


flatten'' :: forall repr. NodeLineMap repr -> String /\ repr /\ Array (String /\ repr) /\ Array (String /\ repr)
flatten'' = flatten' >>> lmap fst