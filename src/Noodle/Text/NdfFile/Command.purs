module Noodle.Text.NdfFile.Command where

import Prelude

import Data.Semigroup ((<>))
import Data.String as String
import Data.Array as Array
import Data.Either (Either(..))

import Type.Proxy (Proxy)

import Noodle.Text.ToCode (class ToCode, class ToTaggedCode, toCode, toTaggedCode, NDF, ndf)

import Data.Text.Format (Tag)
import Data.Text.Format as T

import Noodle.Ui.Cli.Tagging as T


newtype NodeFamily = NodeFamily String
newtype NodeId = NodeId String
newtype Coord = Coord Int
newtype InletId = InletId (Either String Int)
newtype OutletId = OutletId (Either String Int)
newtype EncodedValue = EncodedValue String


derive newtype instance Eq NodeFamily
derive newtype instance Eq NodeId
derive newtype instance Eq Coord
derive newtype instance Eq InletId
derive newtype instance Eq OutletId
derive newtype instance Eq EncodedValue


data Command
    = MakeNode NodeFamily Coord Coord NodeId
    | Move NodeId Coord Coord
    | Connect NodeId OutletId NodeId InletId
    | Send NodeId InletId EncodedValue
    | SendO NodeId OutletId EncodedValue
    | Comment String


derive instance Eq Command


instance ToCode NDF Command where
    toCode :: Proxy NDF -> Command -> String
    toCode _ =
        case _ of
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
    toTaggedCode :: Proxy NDF -> Command -> T.Tag -- TODO: move to Cli.Tagging module
    toTaggedCode _ =
        case _ of
            MakeNode (NodeFamily family) (Coord top) (Coord left) (NodeId nodeId) -> T.family family <> T.s " " <> T.coord top <> T.s " " <> T.coord left <> T.s " " <> T.nodeId nodeId
            Move  (NodeId nodeId) (Coord top) (Coord left) -> T.operator "." <> T.s " " <> T.coord top <> T.s " " <> T.coord left <> T.s " " <> T.nodeId nodeId
            Send  (NodeId nodeId) (InletId (Right iindex))  (EncodedValue value) -> T.operator "->" <> T.s " " <> T.nodeId nodeId <> T.s " " <> T.inletIdx iindex  <> T.s " " <> T.value value
            Send  (NodeId nodeId) (InletId (Left iname))    (EncodedValue value) -> T.operator "->" <> T.s " " <> T.nodeId nodeId <> T.s " " <> T.inletId iname    <> T.s " " <> T.value value
            SendO (NodeId nodeId) (OutletId (Right oindex)) (EncodedValue value) -> T.operator "~>" <> T.s " " <> T.nodeId nodeId <> T.s " " <> T.outletIdx oindex <> T.s " " <> T.value value
            SendO (NodeId nodeId) (OutletId (Left oname))   (EncodedValue value) -> T.operator "~>" <> T.s " " <> T.nodeId nodeId <> T.s " " <> T.outletId oname   <> T.s " " <> T.value value
            Connect (NodeId fromNode) (OutletId (Right oindex)) (NodeId toNode) (InletId (Right iindex)) -> T.operator "<>" <> T.s " " <> T.nodeId fromNode <> T.s " " <> T.outletIdx oindex <> T.s " " <> T.nodeId toNode <> T.s " " <> T.inletIdx iindex
            Connect (NodeId fromNode) (OutletId (Left oname))   (NodeId toNode) (InletId (Left iname))   -> T.operator "<>" <> T.s " " <> T.nodeId fromNode <> T.s " " <> T.outletId oname <> T.s " " <> T.nodeId toNode <> T.s " " <> T.inletId iname
            Connect (NodeId fromNode) (OutletId (Right oindex)) (NodeId toNode) (InletId (Left iname))   -> T.operator "<>" <> T.s " " <> T.nodeId fromNode <> T.s " " <> T.outletIdx oindex <> T.s " " <> T.nodeId toNode <> T.s " " <> T.inletId iname
            Connect (NodeId fromNode) (OutletId (Left oname))   (NodeId toNode) (InletId (Right iindex)) -> T.operator "<>" <> T.s " " <> T.nodeId fromNode <> T.s " " <> T.outletId oname <> T.s " " <> T.nodeId toNode <> T.s " " <> T.inletIdx iindex
            Comment content -> T.comment $ "# " <> content


-- instance ToCode NDF (Array Command) where
commandsToNdf :: Array Command -> String
commandsToNdf cmds = String.joinWith "\n" $ toCode ndf <$> (optimize $ Array.reverse cmds)


-- instance ToCode NDF (Array Command) where
commandsToTaggedNdf :: Array Command -> T.Tag
commandsToTaggedNdf cmds = T.joinWith T.nl $ toTaggedCode ndf <$> (optimize $ Array.reverse cmds)


optimize :: Array Command -> Array Command
optimize = identity -- TODO


family :: String -> NodeFamily
family = NodeFamily


nodeId :: String -> NodeId
nodeId = NodeId


coord :: Int -> Coord
coord = Coord


inletIndex :: Int -> InletId
inletIndex = InletId <<< Right


inletAlias :: String -> InletId
inletAlias = InletId <<< Left


outletIndex :: Int -> OutletId
outletIndex = OutletId <<< Right


outletAlias :: String -> OutletId
outletAlias = OutletId <<< Left


encodedValue :: String -> EncodedValue
encodedValue = EncodedValue


-- instance ToFn