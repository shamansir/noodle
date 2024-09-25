module Noodle.Text.NdfFile.Command where

import Prelude

import Data.Semigroup ((<>))
import Data.String as String
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

import Data.Text.Format (Tag)
import Data.Text.Format as T

import Type.Proxy (Proxy)

import Noodle.Fn.ToFn (Fn, toFn, Argument, Output, argName, argValue, outName, outValue)
import Noodle.Text.ToCode (class ToCode, class ToTaggedCode, toCode, toTaggedCode, NDF, ndf)

import Noodle.Ui.Cli.Tagging as F


newtype FamilyGroup = FamilyGroup String
newtype NodeFamily = NodeFamily String
newtype NodeId = NodeId String
newtype Coord = Coord Int
newtype InletId = InletId (Either String Int)
newtype OutletId = OutletId (Either String Int)
newtype EncodedType = EncodedType String
newtype EncodedValue = EncodedValue String
newtype ProcessCode = ProcessCode String


derive newtype instance Eq FamilyGroup
derive newtype instance Eq NodeFamily
derive newtype instance Eq NodeId
derive newtype instance Eq Coord
derive newtype instance Eq InletId
derive newtype instance Eq OutletId
derive newtype instance Eq EncodedType
derive newtype instance Eq EncodedValue
derive newtype instance Eq ProcessCode


data Command
    = DefineNode FamilyGroup NodeFamily NodeFnDef (Maybe ProcessCode)
    | AssignProcess NodeFamily ProcessCode
    | MakeNode NodeFamily Coord Coord NodeId
    | Move NodeId Coord Coord
    | Connect NodeId OutletId NodeId InletId
    | Send NodeId InletId EncodedValue
    | SendO NodeId OutletId EncodedValue
    | Comment String


derive instance Eq Command


type InletDef = { dataType :: EncodedType, defaultValue :: Maybe EncodedValue }
type OutletDef = { dataType :: EncodedType, defaultValue :: Maybe EncodedValue }


newtype NodeFnDef = NodeFnDef (Fn InletDef OutletDef)
derive newtype instance Eq NodeFnDef


instance ToCode NDF Command where
    toCode :: Proxy NDF -> Command -> String
    toCode pndf =
        case _ of
            DefineNode (FamilyGroup group) (NodeFamily family) (NodeFnDef fnDef) (Just (ProcessCode processCode)) ->
                ": " <> group <> " : " <> family <> " :: " <> toCode pndf (NodeFnDef fnDef) <> " /-|" <> processCode <> "|-/"
            DefineNode (FamilyGroup group) (NodeFamily family) (NodeFnDef fnDef) Nothing ->
                ": " <> group <> " : " <> family <> " :: " <> toCode pndf (NodeFnDef fnDef)
            AssignProcess (NodeFamily family) (ProcessCode processCode) ->
                "$ " <> "/-|" <> processCode <> "|-/"
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
            DefineNode (FamilyGroup group) (NodeFamily family) (NodeFnDef fnDef) (Just (ProcessCode processCode)) ->
                F.operator ":"
                <> T.s " " <> F.someGroup group <> T.s " " <> F.operator ":"
                <> T.s " " <> F.family family <> T.s " " <> F.operator "::"
                <> T.s " " <> toTaggedCode pndf (NodeFnDef fnDef)
                <> T.s " " <> F.operator "/-|" <> T.s processCode <> F.operator " |-/"
            DefineNode (FamilyGroup group) (NodeFamily family) (NodeFnDef fnDef) Nothing ->
                F.operator ":"
                <> T.s " " <> F.someGroup group <> T.s " " <> F.operator ":"
                <> T.s " " <> F.family family <> T.s " " <> F.operator "::"
                <> T.s " " <> toTaggedCode pndf (NodeFnDef fnDef)
            AssignProcess (NodeFamily family) (ProcessCode processCode) ->
                F.operator "$"
                <> T.s " " <> F.family family <> T.s " " <> F.operator "::"
                <> T.s " " <> F.operator "/-|" <> T.s processCode <> F.operator " |-/"
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


instance ToCode NDF NodeFnDef where
    toCode :: Proxy NDF -> NodeFnDef -> String
    toCode _ =
        case _ of
            NodeFnDef fn ->
                case (toFn fn :: String /\ Array (Argument InletDef) /\ Array (Output OutletDef)) of
                    (_ /\ inlets /\ outlets) ->
                        inletsList inlets <>
                        " => " <>
                        case outlets of
                            [ singleOutput ] ->
                                if outName singleOutput == "out" then outletDefToCode singleOutput else outletsList outlets
                            _ -> outletsList outlets
        where
            inletsList inlets = "<" <> String.joinWith " -> " (inletDefToCode <$> inlets) <> ">"
            outletsList outlets = "<" <> String.joinWith " -> " (outletDefToCode <$> outlets) <> ">"
            inletDefToCode :: Argument InletDef -> String
            inletDefToCode arg =
                case argValue arg of
                    { dataType, defaultValue } ->
                        case (dataType /\ defaultValue) of
                            EncodedType dataTypeStr /\ Just (EncodedValue valueStr) ->
                                argName arg <> ":" <> dataTypeStr <> " " <> "{" <> valueStr <> "}"
                            EncodedType dataTypeStr /\ Nothing ->
                                argName arg <> ":" <> dataTypeStr
            outletDefToCode :: Output OutletDef -> String
            outletDefToCode out =
                case outValue out of
                    { dataType, defaultValue } ->
                        case (dataType /\ defaultValue) of
                            EncodedType dataTypeStr /\ Just (EncodedValue valueStr) ->
                                outName out <> ":" <> dataTypeStr <> " " <> "{" <> valueStr <> "}"
                            EncodedType dataTypeStr /\ Nothing ->
                                outName out <> ":" <> dataTypeStr


instance ToTaggedCode NDF NodeFnDef where
    toTaggedCode :: Proxy NDF -> NodeFnDef -> T.Tag
    toTaggedCode _ =
        case _ of
            NodeFnDef fn ->
                case (toFn fn :: String /\ Array (Argument InletDef) /\ Array (Output OutletDef)) of
                    (_ /\ inlets /\ outlets) ->
                        inletsList inlets <>
                        T.s " " <> F.operator "=>" <> T.s " " <>
                        case outlets of
                            [ singleOutput ] ->
                                if outName singleOutput == "out" then outletDefToCode singleOutput else outletsList outlets
                            _ -> outletsList outlets
        where
            inletsList inlets = F.operator "<" <> T.joinWith (T.s " " <> F.operator "->" <> T.s " ") (inletDefToCode <$> inlets) <> F.operator ">"
            outletsList outlets = F.operator "<" <> T.joinWith (T.s " " <> F.operator "->" <> T.s " ") (outletDefToCode <$> outlets) <> F.operator ">"
            inletDefToCode :: Argument InletDef -> T.Tag
            inletDefToCode arg =
                case argValue arg of
                    { dataType, defaultValue } ->
                        case (dataType /\ defaultValue) of
                            EncodedType dataTypeStr /\ Just (EncodedValue valueStr) ->
                                (F.outletId $ argName arg) <> F.operator ":" <> F.type_ dataTypeStr <> T.s " " <> F.operator "{" <> F.value valueStr <> F.operator "}"
                            EncodedType dataTypeStr /\ Nothing ->
                                (F.outletId $ argName arg) <> F.operator ":" <> F.type_ dataTypeStr
            outletDefToCode :: Output OutletDef -> T.Tag
            outletDefToCode out =
                case outValue out of
                    { dataType, defaultValue } ->
                        case (dataType /\ defaultValue) of
                            EncodedType dataTypeStr /\ Just (EncodedValue valueStr) ->
                                (F.outletId $ outName out) <> F.operator ":" <> F.type_ dataTypeStr <> T.s " " <> F.operator "{" <> F.value valueStr <> F.operator "}"
                            EncodedType dataTypeStr /\ Nothing ->
                                (F.outletId $ outName out) <> F.operator ":" <> F.type_ dataTypeStr


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