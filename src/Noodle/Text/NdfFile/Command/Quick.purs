module Noodle.Text.NdfFile.Command.Quick where

import Prelude

import Data.UniqueHash as UH
import Data.String (drop) as String

import Noodle.Id (NodeR, hashOf, family, familyOf) as Id

import Noodle.Text.NdfFile.Types as T
import Noodle.Text.NdfFile.Command.Op as Op


instanceIdFor :: Id.NodeR -> T.NodeInstanceId
instanceIdFor nodeR = T.NodeInstanceId $ (Id.family $ Id.familyOf nodeR) <> "-" <> (String.drop 12 $ UH.toString $ Id.hashOf nodeR)


makeNode :: Id.NodeR -> { left :: Int, top :: Int } -> Op.CommandOp
makeNode nodeR { left, top } = Op.MakeNode (Id.familyOf nodeR) (T.Coord left) (T.Coord top) $ instanceIdFor nodeR