module Noodle.Text.NdfFile.Command where

import Prelude

import Data.Semigroup ((<>))
import Data.String as String
import Data.Array as Array
import Data.Either (Either(..))

import Type.Proxy (Proxy)

import Tookit.Hydra.Lang.ToCode (class ToCode, class ToTaggedCode, toCode, toTaggedCode, NDF, ndf)

import Blessed.Tagger (Tag)
import Blessed.Tagger as T
import Cli.Tagging as T


newtype NodeFamily = NodeFamily String
newtype NodeId = NodeId String
newtype Coord = Coord Int
newtype InputId = InputId (Either String Int)
newtype OutputId = OutputId (Either String Int)
newtype EncodedValue = EncodedValue String


derive newtype instance Eq NodeFamily
derive newtype instance Eq NodeId
derive newtype instance Eq Coord
derive newtype instance Eq InputId
derive newtype instance Eq OutputId
derive newtype instance Eq EncodedValue


data Command
    = MakeNode NodeFamily Coord Coord NodeId
    | Move NodeId Coord Coord
    | Connect NodeId OutputId NodeId InputId
    | Send NodeId InputId EncodedValue
    | SendO NodeId OutputId EncodedValue
    | Comment String


derive instance Eq Command


instance ToCode NDF Command where
    toCode :: Proxy NDF -> Command -> String
    toCode _ =
        case _ of
            MakeNode (NodeFamily family) (Coord top) (Coord left) (NodeId nodeId) -> family <> " " <> show top <> " " <> show left <> " " <> nodeId
            Move  (NodeId nodeId) (Coord top) (Coord left) -> ". " <> show top <> " " <> show left <> " " <> nodeId
            Send  (NodeId nodeId) (InputId (Right iindex))  (EncodedValue value) -> "-> " <> nodeId <> " " <> show iindex <> " " <> value
            Send  (NodeId nodeId) (InputId (Left iname))    (EncodedValue value) -> "-> " <> nodeId <> " " <> iname <> " " <> value
            SendO (NodeId nodeId) (OutputId (Right oindex)) (EncodedValue value) -> "~> " <> nodeId <> " " <> show oindex <> " " <> value
            SendO (NodeId nodeId) (OutputId (Left oname))   (EncodedValue value) -> "~> " <> nodeId <> " " <> oname <> " " <> value
            Connect (NodeId fromNode) (OutputId (Right oindex)) (NodeId toNode) (InputId (Right iindex)) -> "<> " <> fromNode <> " " <> show oindex <> " " <> toNode <> " " <> show iindex
            Connect (NodeId fromNode) (OutputId (Left oname))   (NodeId toNode) (InputId (Left iname))   -> "<> " <> fromNode <> " " <> oname <> " " <> toNode <> " " <> iname
            Connect (NodeId fromNode) (OutputId (Right oindex)) (NodeId toNode) (InputId (Left iname))   -> "<> " <> fromNode <> " " <> show oindex <> " " <> toNode <> " " <> iname
            Connect (NodeId fromNode) (OutputId (Left oname))   (NodeId toNode) (InputId (Right iindex)) -> "<> " <> fromNode <> " " <> oname <> " " <> toNode <> " " <> show iindex
            Comment content -> "# " <> content


instance ToTaggedCode NDF Command where
    toTaggedCode :: Proxy NDF -> Command -> T.Tag -- TODO: move to Cli.Tagging module
    toTaggedCode _ =
        case _ of
            MakeNode (NodeFamily family) (Coord top) (Coord left) (NodeId nodeId) -> T.family family <> T.s " " <> T.coord top <> T.s " " <> T.coord left <> T.s " " <> T.nodeId nodeId
            Move  (NodeId nodeId) (Coord top) (Coord left) -> T.operator "." <> T.s " " <> T.coord top <> T.s " " <> T.coord left <> T.s " " <> T.nodeId nodeId
            Send  (NodeId nodeId) (InputId (Right iindex))  (EncodedValue value) -> T.operator "->" <> T.s " " <> T.nodeId nodeId <> T.s " " <> T.inputIdx iindex  <> T.s " " <> T.value value
            Send  (NodeId nodeId) (InputId (Left iname))    (EncodedValue value) -> T.operator "->" <> T.s " " <> T.nodeId nodeId <> T.s " " <> T.inputId iname    <> T.s " " <> T.value value
            SendO (NodeId nodeId) (OutputId (Right oindex)) (EncodedValue value) -> T.operator "~>" <> T.s " " <> T.nodeId nodeId <> T.s " " <> T.outputIdx oindex <> T.s " " <> T.value value
            SendO (NodeId nodeId) (OutputId (Left oname))   (EncodedValue value) -> T.operator "~>" <> T.s " " <> T.nodeId nodeId <> T.s " " <> T.outputId oname   <> T.s " " <> T.value value
            Connect (NodeId fromNode) (OutputId (Right oindex)) (NodeId toNode) (InputId (Right iindex)) -> T.operator "<>" <> T.s " " <> T.nodeId fromNode <> T.s " " <> T.outputIdx oindex <> T.s " " <> T.nodeId toNode <> T.s " " <> T.inputIdx iindex
            Connect (NodeId fromNode) (OutputId (Left oname))   (NodeId toNode) (InputId (Left iname))   -> T.operator "<>" <> T.s " " <> T.nodeId fromNode <> T.s " " <> T.outputId oname <> T.s " " <> T.nodeId toNode <> T.s " " <> T.inputId iname
            Connect (NodeId fromNode) (OutputId (Right oindex)) (NodeId toNode) (InputId (Left iname))   -> T.operator "<>" <> T.s " " <> T.nodeId fromNode <> T.s " " <> T.outputIdx oindex <> T.s " " <> T.nodeId toNode <> T.s " " <> T.inputId iname
            Connect (NodeId fromNode) (OutputId (Left oname))   (NodeId toNode) (InputId (Right iindex)) -> T.operator "<>" <> T.s " " <> T.nodeId fromNode <> T.s " " <> T.outputId oname <> T.s " " <> T.nodeId toNode <> T.s " " <> T.inputIdx iindex
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


inputIndex :: Int -> InputId
inputIndex = InputId <<< Right


inputAlias :: String -> InputId
inputAlias = InputId <<< Left


outputIndex :: Int -> OutputId
outputIndex = OutputId <<< Right


outputAlias :: String -> OutputId
outputAlias = OutputId <<< Left


encodedValue :: String -> EncodedValue
encodedValue = EncodedValue


-- instance ToFn