module Noodle.Text.NdfFile.NodeDef where

import Prelude

import Type.Proxy (Proxy)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith) as String
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Text.Format as T

import Noodle.Text.ToCode (class ToCode, toCode, class ToTaggedCode, toTaggedCode, NDF, PS)
import Noodle.Fn.ToFn (fn, Fn, toFn, Argument, Output, argName, argValue, outName, outValue, arg, out)
import Noodle.Fn.ToFn (name) as Fn
import Noodle.Text.NdfFile.Newtypes (EncodedType(..), EncodedValue(..), FamilyGroup, NodeFamily, ProcessCode, ChannelDef(..), StateDef(..), emptyStateDef)
import Noodle.Text.NdfFile.NodeDef.Codegen (class CodegenRepr)
import Noodle.Text.NdfFile.NodeDef.Codegen as CodeGen
import Noodle.Ui.Cli.Tagging as F


newtype NodeDef = NodeDef
    { group :: FamilyGroup
    , fn :: NodeFnDef
    , state :: StateDef
    , process :: Maybe ProcessCode
    }
derive instance Newtype NodeDef _
derive newtype instance Eq NodeDef


newtype NodeFnDef = NodeFnDef (Fn ChannelDef ChannelDef)
derive instance Newtype NodeFnDef _
derive newtype instance Eq NodeFnDef


newtype ProcessAssign =
    ProcessAssign (NodeFamily /\ ProcessCode)
derive instance Newtype ProcessAssign _
derive newtype instance Eq ProcessAssign


instance ToCode NDF opts NodeFnDef where
    toCode :: Proxy NDF -> opts -> NodeFnDef -> String
    toCode _ _ =
        case _ of
            NodeFnDef fn ->
                case (toFn fn :: String /\ Array (Argument ChannelDef) /\ Array (Output ChannelDef)) of
                    (_ /\ inlets /\ outlets) ->
                        inletsList inlets <>
                        " => " <>
                        case outlets of
                            [ singleOutput ] -> outletDefToCode singleOutput
                            _ -> outletsList outlets
        where
            inletsList inlets = "<" <> String.joinWith " -> " (inletDefToCode <$> inlets) <> ">"
            outletsList outlets = "<" <> String.joinWith " -> " (outletDefToCode <$> outlets) <> ">"
            inletDefToCode :: Argument ChannelDef -> String
            inletDefToCode arg =
                channelToCode_ (argName arg) (argValue arg)
            outletDefToCode :: Output ChannelDef -> String
            outletDefToCode out =
                channelToCode_ (outName out) (outValue out)


instance ToTaggedCode NDF opts NodeFnDef where
    toTaggedCode :: Proxy NDF -> opts -> NodeFnDef -> T.Tag
    toTaggedCode _ _ =
        case _ of
            NodeFnDef fn ->
                case (toFn fn :: String /\ Array (Argument ChannelDef) /\ Array (Output ChannelDef)) of
                    (_ /\ inlets /\ outlets) ->
                        inletsList inlets <>
                        T.s " " <> F.operator "=>" <> T.s " " <>
                        case outlets of
                            [ singleOutput ] -> outletDefToTaggedCode singleOutput
                            _ -> outletsList outlets
        where
            inletsList inlets = F.operator "<" <> T.joinWith (T.s " " <> F.operator "->" <> T.s " ") (inletDefToTaggedCode <$> inlets) <> F.operator ">"
            outletsList outlets = F.operator "<" <> T.joinWith (T.s " " <> F.operator "->" <> T.s " ") (outletDefToTaggedCode <$> outlets) <> F.operator ">"
            inletDefToTaggedCode :: Argument ChannelDef -> T.Tag
            inletDefToTaggedCode arg =
                channelToTaggedCode_ F.inletId (argName arg) (argValue arg)
            outletDefToTaggedCode :: Output ChannelDef -> T.Tag
            outletDefToTaggedCode out =
                channelToTaggedCode_ F.outletId (outName out) (outValue out)


instance ToCode NDF opts NodeDef where
    toCode :: Proxy NDF -> opts -> NodeDef -> String
    toCode pndf opts (NodeDef ndef) =
        case ndef of
            { group, fn, process } ->
                ": " <> unwrap group <> " : " <> (Fn.name $ unwrap fn) <> " :: " <> toCode pndf opts fn <> case process of
                    Just processCode -> " /-|" <> unwrap processCode <> "|-/"
                    Nothing -> ""


instance ToTaggedCode NDF opts NodeDef where
    toTaggedCode :: Proxy NDF -> opts -> NodeDef -> T.Tag
    toTaggedCode pndf opts (NodeDef ndef) =
        case ndef of
            { group, fn, process } ->
                F.operator ":"
                <> T.s " " <> F.someGroup (unwrap group) <> T.s " " <> F.operator ":"
                <> T.s " " <> F.family (Fn.name $ unwrap fn) <> T.s " " <> F.operator "::"
                <> T.s " " <> toTaggedCode pndf opts fn
                <> case process of
                    Just processCode -> T.s " " <> F.operator "/-|" <> T.s (unwrap processCode) <> F.operator "|-/"
                    Nothing -> T.s ""


instance ToCode NDF opts ProcessAssign where
    toCode :: Proxy NDF -> opts -> ProcessAssign -> String
    toCode _ _ (ProcessAssign padef) =
        case padef of
            family /\ process ->
                "$ " <> unwrap family <> " :: " <> "/-|" <> unwrap process <> "|-/"


instance ToTaggedCode NDF opts ProcessAssign where
    toTaggedCode :: Proxy NDF -> opts -> ProcessAssign -> T.Tag
    toTaggedCode _ _ (ProcessAssign padef) =
        case padef of
            family /\ process ->
                F.operator ":"
                <> T.s " " <> F.family (unwrap family) <> T.s " " <> F.operator "::"
                <> T.s " " <> F.operator "/-|" <> T.s (unwrap process) <> F.operator "|-/"


instance CodegenRepr repr => ToCode PS (CodeGen.Options repr) NodeDef where
    toCode :: Proxy PS -> CodeGen.Options repr -> NodeDef -> String
    toCode _ opts (NodeDef ndef) =
        CodeGen.generate opts ndef.group ndef.state (unwrap ndef.fn)
            $ fromMaybe (wrap "pure unit")
            $ ndef.process


channelToCode_ :: String -> ChannelDef -> String
channelToCode_ chName (ChannelDef { mbType, mbDefault }) =
    case (mbType /\ mbDefault) of
        Just (EncodedType dataTypeStr) /\ Just (EncodedValue valueStr) ->
            chName <> ":" <> dataTypeStr <> " " <> "{" <> valueStr <> "}"
        Just (EncodedType dataTypeStr) /\ Nothing ->
            chName <> ":" <> dataTypeStr
        Nothing /\ Just (EncodedValue valueStr) ->
            chName <> ":" <> "{" <> valueStr <> "}"
        Nothing /\ Nothing ->
            chName


channelToTaggedCode_ :: (String -> T.Tag) -> String -> ChannelDef -> T.Tag
channelToTaggedCode_ nameToTag chName (ChannelDef { mbType, mbDefault }) =
    case (mbType /\ mbDefault) of
        Just (EncodedType dataTypeStr) /\ Just (EncodedValue valueStr) ->
            nameToTag chName <> F.operator ":" <> F.type_ dataTypeStr <> T.s " " <> F.operator "{" <> F.value valueStr <> F.operator "}"
        Just (EncodedType dataTypeStr) /\ Nothing ->
            nameToTag chName <> F.operator ":" <> F.type_ dataTypeStr
        Nothing /\ Just (EncodedValue valueStr) ->
            nameToTag chName <> F.operator ":" <> F.operator "{" <> F.value valueStr <> F.operator "}"
        Nothing /\ Nothing ->
            nameToTag chName



channelToArg :: String -> ChannelDef -> Argument ChannelDef
channelToArg = arg


channelToOut :: String -> ChannelDef -> Output ChannelDef
channelToOut = out


i :: { name :: String, type_ :: Maybe String, value :: Maybe String } -> Argument ChannelDef
i { name, type_, value } = channelToArg name $ ChannelDef { mbType : EncodedType <$> type_, mbDefault : EncodedValue <$> value }


o :: { name :: String, type_ :: Maybe String, value :: Maybe String } -> Output ChannelDef
o { name, type_, value } = channelToOut name $ ChannelDef { mbType : EncodedType <$> type_, mbDefault : EncodedValue <$> value }


ch :: String -> { name :: String, type_ :: Maybe String, value :: Maybe String }
ch name = { name, type_ : Nothing, value : Nothing }


chtv :: String -> String -> String -> { name :: String, type_ :: Maybe String, value :: Maybe String }
chtv name type_ value = { name, type_ : Just type_, value : Just value }


cht :: String -> String -> { name :: String, type_ :: Maybe String, value :: Maybe String }
cht name type_ = { name, type_ : Just type_, value : Nothing }


chv :: String -> String -> { name :: String, type_ :: Maybe String, value :: Maybe String }
chv name value = { name, type_ : Nothing, value : Just value }


st :: String -> String -> StateDef
st type_ value =
    StateDef
        { mbType : Just $ EncodedType type_
        , mbDefault : Just $ EncodedValue value
        }


stt :: String -> StateDef
stt type_ =
    StateDef
        { mbType : Just $ EncodedType type_
        , mbDefault : Nothing
        }


stv :: String -> StateDef
stv value =
    StateDef
        { mbType : Nothing
        , mbDefault : Just $ EncodedValue value
        }


qdef :: { group :: String, family :: String, inputs :: Array (Argument ChannelDef), outputs :: Array (Output ChannelDef) } -> NodeDef
qdef { group, family, inputs, outputs } =
    NodeDef
        { group : wrap group
        , fn : wrap $ fn family inputs outputs
        , state : emptyStateDef
        , process : Nothing
        }


qdefp :: { group :: String, family :: String, inputs :: Array (Argument ChannelDef), outputs :: Array (Output ChannelDef), process :: String } -> NodeDef
qdefp { group, family, inputs, outputs, process } =
    NodeDef
        { group : wrap group
        , fn : wrap $ fn family inputs outputs
        , state : emptyStateDef
        , process : Just $ wrap process
        }


qdefs :: { group :: String, family :: String, inputs :: Array (Argument ChannelDef), outputs :: Array (Output ChannelDef), state :: StateDef } -> NodeDef
qdefs { group, family, inputs, outputs, state } =
    NodeDef
        { group : wrap group
        , fn : wrap $ fn family inputs outputs
        , state
        , process : Nothing
        }


qdefps :: { group :: String, family :: String, inputs :: Array (Argument ChannelDef), outputs :: Array (Output ChannelDef), state :: StateDef, process :: String } -> NodeDef
qdefps { group, family, inputs, outputs, state, process } =
    NodeDef
        { group : wrap group
        , fn : wrap $ fn family inputs outputs
        , state
        , process : Just $ wrap process
        }


qassign :: String -> String -> ProcessAssign
qassign family pcode =
    wrap $ wrap family /\ wrap pcode