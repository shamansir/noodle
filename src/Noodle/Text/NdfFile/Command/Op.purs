module Noodle.Text.NdfFile.Command.Op where

import Prelude

import Data.String as String
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foldable (foldl)

import Data.Text.Format as T

import Type.Proxy (Proxy)

import Foreign (F, Foreign)
import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl)

import Noodle.Id (FamilyR, family) as Id
import Noodle.Text.ToCode (class ToCode, class ToTaggedCode, toCode, toTaggedCode)
import Noodle.Text.Code.Target (NDF, ndf)
import Noodle.Ui.Cli.Tagging as F
import Noodle.Text.NdfFile.Types (Coord(..), EncodedValue(..), InletId(..), NodeInstanceId(..), OutletId(..))
import Noodle.Text.NdfFile.FamilyDef (FamilyDef, ProcessAssign)
import Noodle.Text.NdfFile.FamilyDef (ndfLinesCount, processAssignNdfLinesCount) as FD



-- TODO: type FamiliesOrder = Array (GroupR /\ Array FamilyR)
type FamiliesOrder = Array (Array Id.FamilyR)


-- TODO: store Command source line and position in every command instead of just `FamilyDef`
--       this way, ndfLineCount will also be accessible from source!
data CommandOp
    = DefineFamily FamilyDef
    | AssignProcess ProcessAssign
    | MakeNode Id.FamilyR Coord Coord NodeInstanceId
    | Move NodeInstanceId Coord Coord
    | Connect NodeInstanceId OutletId NodeInstanceId InletId
    | Send NodeInstanceId InletId EncodedValue
    | SendO NodeInstanceId OutletId EncodedValue
    | Order FamiliesOrder
    | Import String
    | Comment String
    -- TODO: | Disconnect
    -- TODO: | Documentation FamilyR String


derive instance Eq CommandOp


instance ToCode NDF opts CommandOp where
    toCode :: Proxy NDF -> opts -> CommandOp -> String
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
            Order items -> "* " <> "| " <> (String.joinWith " | " $ String.joinWith " " <$> map Id.family <$> items) <> " |"


instance ToTaggedCode NDF opts CommandOp where
    toTaggedCode :: Proxy NDF -> opts -> CommandOp -> T.Tag
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
            Order items -> T.mark (F.operator "*") $ T.wrap (F.orderSplit "|") (F.orderSplit "|") $ T.joinWith (T.space <> F.orderSplit "|" <> T.space) $ T.joinWith T.space <$> (map (Id.family >>> F.orderItem) <$> items)


toNdf :: Array CommandOp -> String
toNdf cmds = String.joinWith "\n" $ toCode ndf unit <$> (optimize cmds)


toTaggedNdf :: Array CommandOp -> T.Tag
toTaggedNdf cmds = T.joinWith T.nl $ toTaggedCode ndf unit <$> (optimize cmds)


optimize :: Array CommandOp -> Array CommandOp
optimize =
    _.optimizedCmds <<< foldl foldF { mbPrevCmd : Nothing, optimizedCmds : [] }
    where
        foldF { mbPrevCmd, optimizedCmds } curCmd =
            { mbPrevCmd : Just curCmd
            , optimizedCmds : case mbPrevCmd of
                Just prevCmd ->
                    if overrides prevCmd curCmd then
                        Array.snoc (Array.dropEnd 1 optimizedCmds) curCmd
                    else
                        Array.snoc optimizedCmds curCmd
                Nothing ->
                    Array.snoc optimizedCmds curCmd
            }
        overrides (Move instanceA _ _) (Move instanceB _ _) = instanceA == instanceB
        overrides _ _ = false


ndfLinesCount :: CommandOp -> Int
ndfLinesCount = case _ of
    DefineFamily familyDef -> max 1 $ FD.ndfLinesCount familyDef
    AssignProcess pAssign ->  max 1 $ FD.processAssignNdfLinesCount pAssign
    _ -> 1


priority :: CommandOp -> Int
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
        >>> map (Array.filter (Id.family >>> String.length >>> (_ > 0)) >>> Array.filter (Id.family >>> (_ /= "|")))


-- TODO:
{-
instance WriteForeign CommandOp where
    writeImpl :: CommandOp -> Foreign
    writeImpl =
        case _ of
            DefineFamily familyDef ->
                writeImpl { op : "define", def : writeImpl familyDef }
            AssignProcess processAssign ->
                writeImpl { op : "assign", assign : writeImpl processAssign }
            MakeNode familyR (Coord top) (Coord left) (NodeInstanceId nodeId) ->
                writeImpl { op : "make", family : Id.family familyR, top, left, instance : nodeId }
            Move  (NodeInstanceId nodeId) (Coord top) (Coord left) ->
                writeImpl { op : "move", top, left, instance : nodeId }
            Send  (NodeInstanceId nodeId) (InletId (Right iindex))  (EncodedValue value) ->
                writeImpl { op : "send-i", iindex, instance : nodeId, value } -- TODO: use `WriteForeign` implementation for value?
            Send  (NodeInstanceId nodeId) (InletId (Left iname))    (EncodedValue value) ->
                writeImpl { op : "send-in", iname, instance : nodeId, value } -- TODO: use `WriteForeign` implementation for value?
            SendO (NodeInstanceId nodeId) (OutletId (Right oindex)) (EncodedValue value) ->
                writeImpl { op : "send-o", oindex, instance : nodeId, value } -- TODO: use `WriteForeign` implementation for value?
            SendO (NodeInstanceId nodeId) (OutletId (Left oname))   (EncodedValue value) ->
                writeImpl { op : "send-on", oname, instance : nodeId, value } -- TODO: use `WriteForeign` implementation for value?
            Connect (NodeInstanceId fromNode) (OutletId (Right oindex)) (NodeInstanceId toNode) (InletId (Right iindex)) ->
                writeImpl { op : "connect-ii", oindex, iindex, fromNode, toNode }
            Connect (NodeInstanceId fromNode) (OutletId (Left oname))   (NodeInstanceId toNode) (InletId (Left iname))   ->
                writeImpl { op : "connect-nn", oname, iname, fromNode, toNode }
            Connect (NodeInstanceId fromNode) (OutletId (Right oindex)) (NodeInstanceId toNode) (InletId (Left iname))   ->
                writeImpl { op : "connect-in", oindex, iname, fromNode, toNode }
            Connect (NodeInstanceId fromNode) (OutletId (Left oname))   (NodeInstanceId toNode) (InletId (Right iindex)) ->
                writeImpl { op : "connect-ni", oname, iindex, fromNode, toNode }
            Comment content ->
                writeImpl { op : "comment", content }
            Import path ->
                writeImpl { op : "import", path }
            Order items ->
                writeImpl { op : "order", items }
-}