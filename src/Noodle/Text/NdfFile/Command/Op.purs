module Noodle.Text.NdfFile.Command.Op where

import Prelude

import Data.String as String
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Functor.Extra ((<$$>))

import Data.Text.Format as T

import Type.Proxy (Proxy)

import Foreign (F, Foreign)
import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl)

import Noodle.Id (FamilyR, family) as Id
import Noodle.Text.ToCode (class ToCode, class ToTaggedCode, toCode, toTaggedCode)
import Noodle.Text.Code.Target (NDF, ndf)
import Noodle.Ui.Tagging as F
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
    | Disconnect NodeInstanceId OutletId NodeInstanceId InletId
    | Send NodeInstanceId InletId EncodedValue
    | SendO NodeInstanceId OutletId EncodedValue
    | Order FamiliesOrder
    | Import String
    | Comment String
    | RemoveNode NodeInstanceId
    | Documentation Id.FamilyR String -- TODO: make `FamilyR` optional and treat documentation before family definition as belonging to this family


derive instance Eq CommandOp


instance ToCode NDF opts CommandOp where
    toCode :: Proxy NDF -> opts -> CommandOp -> String
    toCode pndf opts =
        case _ of
            DefineFamily familyDef ->
                toCode pndf opts familyDef
            AssignProcess processAssign ->
                toCode pndf opts processAssign
            MakeNode familyR (Coord top) (Coord left) (NodeInstanceId nodeId)       -> show familyR <> " " <> show top <> " " <> show left <> " " <> nodeId
            RemoveNode (NodeInstanceId nodeId)                                      -> "x " <> nodeId
            Move  (NodeInstanceId nodeId) (Coord top) (Coord left)                  -> ". " <> show top <> " " <> show left <> " " <> nodeId
            Send  (NodeInstanceId nodeId) (InletId eInletId)  (EncodedValue value)  -> "-> " <> nodeId <> " " <> eitherToCode eInletId <> " " <> value
            SendO (NodeInstanceId nodeId) (OutletId eOutletId) (EncodedValue value) -> "~> " <> nodeId <> " " <> eitherToCode eOutletId <> " " <> value
            Connect (NodeInstanceId fromNode) (OutletId eOutletId) (NodeInstanceId toNode) (InletId eInletId)    -> "<> " <> fromNode <> " " <> eitherToCode eOutletId <> " " <> toNode <> " " <> eitherToCode eInletId
            Disconnect (NodeInstanceId fromNode) (OutletId eOutletId) (NodeInstanceId toNode) (InletId eInletId) -> ">< " <> fromNode <> " " <> eitherToCode eOutletId <> " " <> toNode <> " " <> eitherToCode eInletId
            Comment content -> "# " <> content
            Import path -> "i " <> path
            Order items -> "* " <> "| " <> (String.joinWith " | " $ String.joinWith " " <$> Id.family <$$> items) <> " |"
            Documentation familyR docLine -> "@ " <> show familyR <> " : " <> docLine
        where
            eitherToCode (Right index) = show index
            eitherToCode (Left  name)  = name


instance ToTaggedCode NDF opts CommandOp where
    toTaggedCode :: Proxy NDF -> opts -> CommandOp -> T.Tag
    toTaggedCode pndf opts =
        case _ of
            DefineFamily familyDef ->
                toTaggedCode pndf opts familyDef
            AssignProcess processAssign ->
                toTaggedCode pndf opts processAssign
            MakeNode familyR (Coord top) (Coord left) (NodeInstanceId nodeId) -> F.family (Id.family familyR) <> T.space <> F.coord top <> T.space <> F.coord left <> T.space <> F.nodeId nodeId
            RemoveNode (NodeInstanceId nodeId) -> F.operator "x" <> T.space <> F.nodeId nodeId
            Move  (NodeInstanceId nodeId) (Coord top) (Coord left) -> F.operator "." <> T.space <> F.coord top <> T.space <> F.coord left <> T.space <> F.nodeId nodeId
            Send  (NodeInstanceId nodeId) eInletId  (EncodedValue value) -> F.operator "->" <> T.space <> F.nodeId nodeId <> T.space <> eInletToCode eInletId   <> T.space <> F.value value
            SendO (NodeInstanceId nodeId) eOutletId (EncodedValue value) -> F.operator "~>" <> T.space <> F.nodeId nodeId <> T.space <> eOutletToCode eOutletId <> T.space <> F.value value
            Connect    (NodeInstanceId fromNode) eOutletId (NodeInstanceId toNode) eInletId -> F.operator "<>" <> T.space <> F.nodeId fromNode <> T.space <> eOutletToCode eOutletId <> T.space <> F.nodeId toNode <> T.space <> eInletToCode eInletId
            Disconnect (NodeInstanceId fromNode) eOutletId (NodeInstanceId toNode) eInletId -> F.operator "><" <> T.space <> F.nodeId fromNode <> T.space <> eOutletToCode eOutletId <> T.space <> F.nodeId toNode <> T.space <> eInletToCode eInletId
            Comment content -> T.mark (T.s "#") $ F.comment content
            Import path -> T.mark (F.operator "i") $ F.filePath path
            Order items -> T.mark (F.operator "*") $ T.wrap (F.orderSplit "|") (F.orderSplit "|") $ T.joinWith (T.space <> F.orderSplit "|" <> T.space) $ T.joinWith T.space <$> ((Id.family >>> F.orderItem) <$$> items)
            Documentation familyR docLine -> F.operator "@" <> T.space <> F.family (Id.family familyR) <> T.space <> F.operator ":" <> T.space <> F.documentation docLine
        where
            eInletToCode  (InletId  (Right iindex)) = F.inletIdx iindex
            eInletToCode  (InletId  (Left  iname))  = F.inletId iname
            eOutletToCode (OutletId (Right iindex)) = F.outletIdx iindex
            eOutletToCode (OutletId (Left  iname))  = F.outletId iname


toNdf :: Array CommandOp -> String
toNdf cmds = String.joinWith "\n" $ toCode ndf unit <$> (optimize cmds)


toTaggedNdf :: Array CommandOp -> T.Tag
toTaggedNdf cmds = T.joinWith T.nl $ toTaggedCode ndf unit <$> (optimize cmds)


-- TODO: - remove disconnect after the same immediate connect
-- TODO: - remove disconnect after the same connect before and without sending values
-- TODO: - remove removing nodes w/o connecting them to anything
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


_priority :: CommandOp -> Int
_priority = case _ of
    Order _ -> 1
    DefineFamily _ -> 2
    Documentation _ _ -> 3 -- while documentation lines are bound to the respecting FamilyR each, we can move it over the file
    AssignProcess _ -> 5 -- assigning process comes after family definitions, but could be placed in the end of file w/o any follow-backs
    -- all the commands below should keep their order in file
    Import _ -> 4 -- FIXME: may be replace imported files with the commands inside them during pre-processing?
    MakeNode _ _ _ _ -> 4
    RemoveNode _ -> 4
    Move _ _ _ -> 4
    Connect _ _ _ _ -> 4
    Disconnect _ _ _ _ -> 4
    Send _ _ _ -> 4
    SendO _ _ _ -> 4
    Comment _ -> 4 -- to keep comments where they belong


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