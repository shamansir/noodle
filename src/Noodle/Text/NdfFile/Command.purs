module Noodle.Text.NdfFile.Command where

import Prelude

import Data.String as String
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

import Data.Text.Format as T

import Type.Proxy (Proxy)

import Noodle.Text.ToCode (class ToCode, class ToTaggedCode, toCode, toTaggedCode, NDF, ndf)
import Noodle.Ui.Cli.Tagging as F
import Noodle.Text.NdfFile.Newtypes
import Noodle.Text.NdfFile.NodeDef (NodeDef, ProcessAssign)


data Command
    = DefineNode NodeDef
    | AssignProcess ProcessAssign
    | MakeNode NodeFamily Coord Coord NodeId
    | Move NodeId Coord Coord
    | Connect NodeId OutletId NodeId InletId
    | Send NodeId InletId EncodedValue
    | SendO NodeId OutletId EncodedValue
    | Comment String


derive instance Eq Command


instance ToCode NDF Command where
    toCode :: Proxy NDF -> Command -> String
    toCode pndf =
        case _ of
            DefineNode nodeDef ->
                toCode pndf nodeDef
            AssignProcess processAssign ->
                toCode pndf processAssign
            MakeNode (NodeFamily family) (Coord top) (Coord left) (NodeId nodeId) -> family <> " " <> show top <> " " <> show left <> " " <> nodeId
            Move  (NodeId nodeId) (Coord top) (Coord left) -> ". " <> show top <> " " <> show left <> " " <> nodeId
            Send  (NodeId nodeId) (InletId (Right iindex))  (EncodedValue value) -> "-> " <> nodeId <> " " <> show iindex <> " " <> value
            Send  (NodeId nodeId) (InletId (Left iname))    (EncodedValue value) -> "-> " <> nodeId <> " " <> iname <> " " <> value
            SendO (NodeId nodeId) (OutletId (Right oindex)) (EncodedValue value) -> "~> " <> nodeId <> " " <> show oindex <> " " <> value
            SendO (NodeId nodeId) (OutletId (Left oname))   (EncodedValue value) -> "~> " <> nodeId <> " " <> oname <> " " <> value
            Connect (NodeId fromNode) (OutletId (Right oindex)) (NodeId toNode) (InletId (Right iindex)) -> "<> " <> fromNode <> " " <> show oindex <> " " <> toNode <> " " <> show iindex
            Connect (NodeId fromNode) (OutletId (Left oname))   (NodeId toNode) (InletId (Left iname))   -> "<> " <> fromNode <> " " <> oname <> " " <> toNode <> " " <> iname
            Connect (NodeId fromNode) (OutletId (Right oindex)) (NodeId toNode) (InletId (Left iname))   -> "<> " <> fromNode <> " " <> show oindex <> " " <> toNode <> " " <> iname
            Connect (NodeId fromNode) (OutletId (Left oname))   (NodeId toNode) (InletId (Right iindex)) -> "<> " <> fromNode <> " " <> oname <> " " <> toNode <> " " <> show iindex
            Comment content -> "# " <> content


instance ToTaggedCode NDF Command where
    toTaggedCode :: Proxy NDF -> Command -> T.Tag
    toTaggedCode pndf =
        case _ of
            DefineNode nodeDef ->
                toTaggedCode pndf nodeDef
            AssignProcess processAssign ->
                toTaggedCode pndf processAssign
            MakeNode (NodeFamily family) (Coord top) (Coord left) (NodeId nodeId) -> F.family family <> T.s " " <> F.coord top <> T.s " " <> F.coord left <> T.s " " <> F.nodeId nodeId
            Move  (NodeId nodeId) (Coord top) (Coord left) -> F.operator "." <> T.s " " <> F.coord top <> T.s " " <> F.coord left <> T.s " " <> F.nodeId nodeId
            Send  (NodeId nodeId) (InletId (Right iindex))  (EncodedValue value) -> F.operator "->" <> T.s " " <> F.nodeId nodeId <> T.s " " <> F.inletIdx iindex  <> T.s " " <> F.value value
            Send  (NodeId nodeId) (InletId (Left iname))    (EncodedValue value) -> F.operator "->" <> T.s " " <> F.nodeId nodeId <> T.s " " <> F.inletId iname    <> T.s " " <> F.value value
            SendO (NodeId nodeId) (OutletId (Right oindex)) (EncodedValue value) -> F.operator "~>" <> T.s " " <> F.nodeId nodeId <> T.s " " <> F.outletIdx oindex <> T.s " " <> F.value value
            SendO (NodeId nodeId) (OutletId (Left oname))   (EncodedValue value) -> F.operator "~>" <> T.s " " <> F.nodeId nodeId <> T.s " " <> F.outletId oname   <> T.s " " <> F.value value
            Connect (NodeId fromNode) (OutletId (Right oindex)) (NodeId toNode) (InletId (Right iindex)) -> F.operator "<>" <> T.s " " <> F.nodeId fromNode <> T.s " " <> F.outletIdx oindex <> T.s " " <> F.nodeId toNode <> T.s " " <> F.inletIdx iindex
            Connect (NodeId fromNode) (OutletId (Left oname))   (NodeId toNode) (InletId (Left iname))   -> F.operator "<>" <> T.s " " <> F.nodeId fromNode <> T.s " " <> F.outletId oname <> T.s " " <> F.nodeId toNode <> T.s " " <> F.inletId iname
            Connect (NodeId fromNode) (OutletId (Right oindex)) (NodeId toNode) (InletId (Left iname))   -> F.operator "<>" <> T.s " " <> F.nodeId fromNode <> T.s " " <> F.outletIdx oindex <> T.s " " <> F.nodeId toNode <> T.s " " <> F.inletId iname
            Connect (NodeId fromNode) (OutletId (Left oname))   (NodeId toNode) (InletId (Right iindex)) -> F.operator "<>" <> T.s " " <> F.nodeId fromNode <> T.s " " <> F.outletId oname <> T.s " " <> F.nodeId toNode <> T.s " " <> F.inletIdx iindex
            Comment content -> F.comment $ "# " <> content


-- instance ToCode NDF (Array Command) where
commandsToNdf :: Array Command -> String
commandsToNdf cmds = String.joinWith "\n" $ toCode ndf <$> (optimize $ Array.reverse cmds)


-- instance ToCode NDF (Array Command) where
commandsToTaggedNdf :: Array Command -> T.Tag
commandsToTaggedNdf cmds = T.joinWith T.nl $ toTaggedCode ndf <$> (optimize $ Array.reverse cmds)


optimize :: Array Command -> Array Command
optimize = identity -- TODO
