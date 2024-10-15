module Noodle.Text.NdfFile.NodeDef where

import Prelude

import Type.Proxy (Proxy)

import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (joinWith) as String
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Text.Format as T

import Noodle.Text.ToCode (class ToCode, toCode, class ToTaggedCode, toTaggedCode)
import Noodle.Text.Code.Target (NDF, PS)
import Noodle.Fn.ToFn (Fn, toFn, Argument, Output, argName, argValue, outName, outValue, arg, out)
import Noodle.Fn.ToFn (name) as Fn
import Noodle.Fn.ToFn (fn) as Make
import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..), FamilyGroup, NodeFamily(..), ChannelDef(..), StateDef(..), emptyStateDef)
import Noodle.Text.NdfFile.NodeDef.ProcessCode (ProcessCode(..))
import Noodle.Text.NdfFile.NodeDef.Codegen (class CodegenRepr)
import Noodle.Text.NdfFile.NodeDef.Codegen as CodeGen
import Noodle.Ui.Cli.Tagging as F


newtype NodeDef = NodeDef
    { group :: FamilyGroup
    , fn :: NodeFnDef
    , state :: StateDef
    , process :: ProcessCode
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
            { group, fn, process, state } ->
                ": " <> unwrap group <> " : " <> (Fn.name $ unwrap fn) <> " :: "
                     <> (if hasStateDef_ state then stateToCode_ state <> " " else "")
                     <> toCode pndf opts fn <> case process of
                        NoneSpecified -> ""
                        _ -> " " <> toCode pndf opts process


instance ToTaggedCode NDF opts NodeDef where
    toTaggedCode :: Proxy NDF -> opts -> NodeDef -> T.Tag
    toTaggedCode pndf opts (NodeDef ndef) =
        case ndef of
            { group, fn, process, state } ->
                F.operator ":"
                <> T.s " " <> F.someGroup (unwrap group) <> T.s " " <> F.operator ":"
                <> T.s " " <> F.family (Fn.name $ unwrap fn) <> T.s " " <> F.operator "::"
                <> (if hasStateDef_ state then T.s " " <> stateToTaggedCode_ state else T.s " ")
                <> toTaggedCode pndf opts fn
                <> case process of
                    NoneSpecified -> T.s ""
                    _ -> T.s " " <> toTaggedCode pndf opts process


instance ToCode NDF opts ProcessAssign where
    toCode :: Proxy NDF -> opts -> ProcessAssign -> String
    toCode pndf opts (ProcessAssign padef) =
        case padef of
            family /\ process ->
                "$ " <> unwrap family <> " :: " <> toCode pndf opts process


instance ToTaggedCode NDF opts ProcessAssign where
    toTaggedCode :: Proxy NDF -> opts -> ProcessAssign -> T.Tag
    toTaggedCode pndf opts (ProcessAssign padef) =
        case padef of
            family /\ process ->
                F.operator ":"
                <> T.s " " <> F.family (unwrap family) <> T.s " " <> F.operator "::"
                <> T.s " " <> toTaggedCode pndf opts process


instance CodegenRepr repr => ToCode PS (CodeGen.Options repr) NodeDef where
    toCode :: Proxy PS -> CodeGen.Options repr -> NodeDef -> String
    toCode _ opts (NodeDef ndef) =
        CodeGen.generate opts ndef.group ndef.state (unwrap ndef.fn) ndef.process


hasStateDef_ :: StateDef -> Boolean
hasStateDef_ (StateDef { mbType, mbDefault }) = isJust mbType || isJust mbDefault


stateToCode_ :: StateDef -> String
stateToCode_ (StateDef { mbType, mbDefault }) =
    case (mbType /\ mbDefault) of
        Just (EncodedType typeStr) /\ Just (EncodedValue valueStr) ->
            "[" <> typeStr <> " " <> "{" <> valueStr <> "}" <> "]"
        Just (EncodedType typeStr) /\ Nothing ->
            "[" <> typeStr <> "]"
        Nothing /\ Just (EncodedValue valueStr) ->
            "[" <> "{" <> valueStr <> "}" <> "]"
        Nothing /\ Nothing ->
            ""


stateToTaggedCode_ :: StateDef -> T.Tag
stateToTaggedCode_ (StateDef { mbType, mbDefault }) =
    case (mbType /\ mbDefault) of
        Just (EncodedType typeStr) /\ Just (EncodedValue valueStr) ->
            F.operator "[" <> F.type_ typeStr <> T.s " " <> F.operator "{" <> F.value valueStr <> F.operator "}" <> F.operator "]"
        Just (EncodedType typeStr) /\ Nothing ->
            F.operator "[" <> F.type_ typeStr <> F.operator "]"
        Nothing /\ Just (EncodedValue valueStr) ->
            F.operator "[" <> F.operator "{" <> F.value valueStr <> F.operator "}" <> F.operator "]"
        Nothing /\ Nothing ->
            T.s ""


channelToCode_ :: String -> ChannelDef -> String
channelToCode_ chName (ChannelDef { mbType, mbDefault }) =
    case (mbType /\ mbDefault) of
        Just (EncodedType typeStr) /\ Just (EncodedValue valueStr) ->
            chName <> ":" <> typeStr <> " " <> "{" <> valueStr <> "}"
        Just (EncodedType typeStr) /\ Nothing ->
            chName <> ":" <> typeStr
        Nothing /\ Just (EncodedValue valueStr) ->
            chName <> ":" <> "{" <> valueStr <> "}"
        Nothing /\ Nothing ->
            chName


channelToTaggedCode_ :: (String -> T.Tag) -> String -> ChannelDef -> T.Tag
channelToTaggedCode_ nameToTag chName (ChannelDef { mbType, mbDefault }) =
    case (mbType /\ mbDefault) of
        Just (EncodedType typeStr) /\ Just (EncodedValue valueStr) ->
            nameToTag chName <> F.operator ":" <> F.type_ typeStr <> T.s " " <> F.operator "{" <> F.value valueStr <> F.operator "}"
        Just (EncodedType typeStr) /\ Nothing ->
            nameToTag chName <> F.operator ":" <> F.type_ typeStr
        Nothing /\ Just (EncodedValue valueStr) ->
            nameToTag chName <> F.operator ":" <> F.operator "{" <> F.value valueStr <> F.operator "}"
        Nothing /\ Nothing ->
            nameToTag chName


group :: NodeDef -> FamilyGroup
group = unwrap >>> _.group


fnDef:: NodeDef -> NodeFnDef
fnDef = unwrap >>> _.fn


family :: NodeDef -> NodeFamily
family = unwrap >>> _.fn >>> unwrap >>> Fn.name >>> NodeFamily


-- familyR :: NodeDef -> Id.FamilyR
-- familyR = family >>> unwrap >>> Id.unsafeFamilyR


stateDef :: NodeDef -> StateDef
stateDef = unwrap >>> _.state


processCode :: NodeDef -> ProcessCode
processCode = unwrap >>> _.process


assignWhenNoneSpecified :: ProcessCode -> NodeDef -> NodeDef
assignWhenNoneSpecified pc nodeDef =
    case pc of
        NoneSpecified -> nodeDef # forceAssign pc
        _ -> nodeDef


forceAssign :: ProcessCode -> NodeDef -> NodeDef
forceAssign pc =
    unwrap >>> _ { process = pc } >>> wrap


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
        , fn : wrap $ Make.fn family inputs outputs
        , state : emptyStateDef
        , process : NoneSpecified
        }


qdefp :: { group :: String, family :: String, inputs :: Array (Argument ChannelDef), outputs :: Array (Output ChannelDef), process :: ProcessCode } -> NodeDef
qdefp { group, family, inputs, outputs, process } =
    NodeDef
        { group : wrap group
        , fn : wrap $ Make.fn family inputs outputs
        , state : emptyStateDef
        , process
        }


qdefs :: { group :: String, family :: String, inputs :: Array (Argument ChannelDef), outputs :: Array (Output ChannelDef), state :: StateDef } -> NodeDef
qdefs { group, family, inputs, outputs, state } =
    NodeDef
        { group : wrap group
        , fn : wrap $ Make.fn family inputs outputs
        , state
        , process : NoneSpecified
        }


qdefps :: { group :: String, family :: String, inputs :: Array (Argument ChannelDef), outputs :: Array (Output ChannelDef), state :: StateDef, process :: ProcessCode } -> NodeDef
qdefps { group, family, inputs, outputs, state, process } =
    NodeDef
        { group : wrap group
        , fn : wrap $ Make.fn family inputs outputs
        , state
        , process
        }


qassign :: String -> ProcessCode -> ProcessAssign
qassign family pcode =
    wrap $ wrap family /\ pcode