module Noodle.Text.NdfFile.Command.Quick where

import Prelude

import Data.UniqueHash as UH
import Data.String (drop) as String
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Id (NodeR, hashOf, family, familyOf) as Id

import Noodle.Text.NdfFile.Types as T
import Noodle.Text.NdfFile.Command.Op as Op
import Noodle.Raw.Link (Link) as Raw
import Noodle.Raw.Link (from, to) as RawLink


instanceIdFor :: Id.NodeR -> T.NodeInstanceId
instanceIdFor nodeR = T.NodeInstanceId $ (Id.family $ Id.familyOf nodeR) <> "-" <> (String.drop 12 $ UH.toString $ Id.hashOf nodeR)


makeNode :: Id.NodeR -> { left :: Int, top :: Int } -> Op.CommandOp
makeNode nodeR { left, top } = Op.MakeNode (Id.familyOf nodeR) (T.Coord left) (T.Coord top) $ instanceIdFor nodeR


moveNode :: Id.NodeR -> { left :: Int, top :: Int } -> Op.CommandOp
moveNode nodeR { left, top } = Op.Move (instanceIdFor nodeR) (T.Coord left) (T.Coord top)


connect :: Raw.Link -> Op.CommandOp
connect link =
    case RawLink.from link /\ RawLink.to link of
        (fromNodeR /\ outletR) /\ (toNodeR /\ inletR) -> Op.Connect (instanceIdFor fromNodeR) (T.fromOutletR outletR) (instanceIdFor toNodeR) (T.fromInletR inletR)


disconnect :: Raw.Link -> Op.CommandOp
disconnect link =
    case RawLink.from link /\ RawLink.to link of
        (fromNodeR /\ outletR) /\ (toNodeR /\ inletR) -> Op.Disconnect (instanceIdFor fromNodeR) (T.fromOutletR outletR) (instanceIdFor toNodeR) (T.fromInletR inletR)


removeNode :: Id.NodeR -> Op.CommandOp
removeNode = instanceIdFor >>> Op.RemoveNode