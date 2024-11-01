module Noodle.Text.NdfFile.Command where

import Prelude

import Data.String as String
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

import Data.Text.Format as T

import Type.Proxy (Proxy)

import Noodle.Id (FamilyR)
import Noodle.Id (family) as Id
import Noodle.Text.ToCode (class ToCode, class ToTaggedCode, toCode, toTaggedCode)
import Noodle.Text.Code.Target (NDF, ndf)
import Noodle.Ui.Cli.Tagging as F
import Noodle.Text.NdfFile.Types
import Noodle.Text.NdfFile.FamilyDef (FamilyDef, ProcessAssign)
import Noodle.Text.NdfFile.FamilyDef (ndfLinesCount, processAssignNdfLinesCount) as FD



-- TODO: type FamiliesOrder = Array (FamilyGroupId /\ Array FamilyId)
type FamiliesOrder = Array (Array String)


data Command
    = DefineFamily FamilyDef
    | AssignProcess ProcessAssign
    | MakeNode FamilyR Coord Coord NodeInstanceId
    | Move NodeInstanceId Coord Coord
    | Connect NodeInstanceId OutletId NodeInstanceId InletId
    | Send NodeInstanceId InletId EncodedValue
    | SendO NodeInstanceId OutletId EncodedValue
    | Order FamiliesOrder
    | Import String
    | Comment String


derive instance Eq Command


instance ToCode NDF opts Command where
    toCode :: Proxy NDF -> opts -> Command -> String
    toCode pndf opts =
        case _ of
            DefineFamily familyDef ->
                toCode pndf opts familyDef
            AssignProcess processAssign ->
                toCode pndf opts processAssign
            MakeNode familyR (Coord top) (Coord left) (NodeInstanceId nodeId) -> show familyR <> " " <> show top <> " " <> show left <> " " <> nodeId
            Move  (NodeInstanceId nodeId) (Coord top) (Coord left) -> ". " <> show top <> " " <> show left <> " " <> nodeId
            Send  (NodeInstanceId nodeId) (InletId (Right iindex))  (EncodedValue value) -> "-> " <> nodeId <> " " <> show iindex <> " " <> value
            Send  (NodeInstanceId nodeId) (InletId (Left iname))    (EncodedValue value) -> "-> " <> nodeId <> " " <> iname <> " " <> value
            SendO (NodeInstanceId nodeId) (OutletId (Right oindex)) (EncodedValue value) -> "~> " <> nodeId <> " " <> show oindex <> " " <> value
            SendO (NodeInstanceId nodeId) (OutletId (Left oname))   (EncodedValue value) -> "~> " <> nodeId <> " " <> oname <> " " <> value
            Connect (NodeInstanceId fromNode) (OutletId (Right oindex)) (NodeInstanceId toNode) (InletId (Right iindex)) -> "<> " <> fromNode <> " " <> show oindex <> " " <> toNode <> " " <> show iindex
            Connect (NodeInstanceId fromNode) (OutletId (Left oname))   (NodeInstanceId toNode) (InletId (Left iname))   -> "<> " <> fromNode <> " " <> oname <> " " <> toNode <> " " <> iname
            Connect (NodeInstanceId fromNode) (OutletId (Right oindex)) (NodeInstanceId toNode) (InletId (Left iname))   -> "<> " <> fromNode <> " " <> show oindex <> " " <> toNode <> " " <> iname
            Connect (NodeInstanceId fromNode) (OutletId (Left oname))   (NodeInstanceId toNode) (InletId (Right iindex)) -> "<> " <> fromNode <> " " <> oname <> " " <> toNode <> " " <> show iindex
            Comment content -> "# " <> content
            Import path -> "i " <> path
            Order items -> "* " <> "| " <> (String.joinWith " | " $ String.joinWith " " <$> items) <> " |"


instance ToTaggedCode NDF opts Command where
    toTaggedCode :: Proxy NDF -> opts -> Command -> T.Tag
    toTaggedCode pndf opts =
        case _ of
            DefineFamily familyDef ->
                toTaggedCode pndf opts familyDef
            AssignProcess processAssign ->
                toTaggedCode pndf opts processAssign
            MakeNode familyR (Coord top) (Coord left) (NodeInstanceId nodeId) -> F.family (Id.family familyR) <> T.space <> F.coord top <> T.space <> F.coord left <> T.space <> F.nodeId nodeId
            Move  (NodeInstanceId nodeId) (Coord top) (Coord left) -> F.operator "." <> T.space <> F.coord top <> T.space <> F.coord left <> T.space <> F.nodeId nodeId
            Send  (NodeInstanceId nodeId) (InletId (Right iindex))  (EncodedValue value) -> F.operator "->" <> T.space <> F.nodeId nodeId <> T.space <> F.inletIdx iindex  <> T.space <> F.value value
            Send  (NodeInstanceId nodeId) (InletId (Left iname))    (EncodedValue value) -> F.operator "->" <> T.space <> F.nodeId nodeId <> T.space <> F.inletId iname    <> T.space <> F.value value
            SendO (NodeInstanceId nodeId) (OutletId (Right oindex)) (EncodedValue value) -> F.operator "~>" <> T.space <> F.nodeId nodeId <> T.space <> F.outletIdx oindex <> T.space <> F.value value
            SendO (NodeInstanceId nodeId) (OutletId (Left oname))   (EncodedValue value) -> F.operator "~>" <> T.space <> F.nodeId nodeId <> T.space <> F.outletId oname   <> T.space <> F.value value
            Connect (NodeInstanceId fromNode) (OutletId (Right oindex)) (NodeInstanceId toNode) (InletId (Right iindex)) -> F.operator "<>" <> T.space <> F.nodeId fromNode <> T.space <> F.outletIdx oindex <> T.space <> F.nodeId toNode <> T.space <> F.inletIdx iindex
            Connect (NodeInstanceId fromNode) (OutletId (Left oname))   (NodeInstanceId toNode) (InletId (Left iname))   -> F.operator "<>" <> T.space <> F.nodeId fromNode <> T.space <> F.outletId oname <> T.space <> F.nodeId toNode <> T.space <> F.inletId iname
            Connect (NodeInstanceId fromNode) (OutletId (Right oindex)) (NodeInstanceId toNode) (InletId (Left iname))   -> F.operator "<>" <> T.space <> F.nodeId fromNode <> T.space <> F.outletIdx oindex <> T.space <> F.nodeId toNode <> T.space <> F.inletId iname
            Connect (NodeInstanceId fromNode) (OutletId (Left oname))   (NodeInstanceId toNode) (InletId (Right iindex)) -> F.operator "<>" <> T.space <> F.nodeId fromNode <> T.space <> F.outletId oname <> T.space <> F.nodeId toNode <> T.space <> F.inletIdx iindex
            Comment content -> T.mark (T.s "#") $ F.comment content
            Import path -> T.mark (F.operator "i") $ F.filePath path
            Order items -> T.mark (F.operator "*") $ T.wrap (F.orderSplit "|") (F.orderSplit "|") $ T.joinWith (T.space <> F.orderSplit "|" <> T.space) $ T.joinWith T.space <$> (map F.orderItem <$> items)


-- instance ToCode NDF (Array Command) where
commandsToNdf :: Array Command -> String
commandsToNdf cmds = String.joinWith "\n" $ toCode ndf unit <$> (optimize cmds)


-- instance ToCode NDF (Array Command) where
commandsToTaggedNdf :: Array Command -> T.Tag
commandsToTaggedNdf cmds = T.joinWith T.nl $ toTaggedCode ndf unit <$> (optimize cmds)


optimize :: Array Command -> Array Command
optimize = identity -- TODO


ndfLinesCount :: Command -> Int
ndfLinesCount = case _ of
    DefineFamily familyDef -> max 1 $ FD.ndfLinesCount familyDef
    AssignProcess pAssign ->  max 1 $ FD.processAssignNdfLinesCount pAssign
    _ -> 1


priority :: Command -> Int
priority = case _ of
    Import _ -> 0
    Order _ -> 1
    DefineFamily _ -> 2
    AssignProcess _ -> 3
    MakeNode _ _ _ _ -> 4
    Move _ _ _ -> 4
    Connect _ _ _ _ -> 4
    Send _ _ _ -> 4
    SendO _ _ _ -> 4
    Comment _ -> 5


reviewOrder_ :: FamiliesOrder -> FamiliesOrder
reviewOrder_ =
    Array.filter (Array.length >>> (_ > 0))
        >>> map (Array.filter (String.length >>> (_ > 0)) >>> Array.filter (_ /= "|"))