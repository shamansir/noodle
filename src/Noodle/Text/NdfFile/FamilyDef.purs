module Noodle.Text.NdfFile.FamilyDef where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (joinWith) as String
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Text.Format as T

import Noodle.Id (GroupR, FamilyR, unsafeFamilyR, unsafeGroupR)
import Noodle.Id (group, family) as Id
import Noodle.Text.ToCode (class ToCode, toCode, class ToTaggedCode, toTaggedCode)
import Noodle.Text.Code.Target (NDF, PS)
import Noodle.Text.FromCode (Source) as FC
import Noodle.Fn.ToFn (class ToFn, Fn, FnS, toFn, Argument, Output, argName, argValue, outName, outValue, arg, out)
import Noodle.Fn.ToFn (name) as Fn
import Noodle.Fn.ToFn (fn) as Make
import Noodle.Text.NdfFile.Types (FamilyDefRec, EncodedType(..), EncodedValue(..), ChannelDef(..), StateDef(..), emptyStateDef)
import Noodle.Text.NdfFile.FamilyDef.ProcessCode (ProcessCode(..))
import Noodle.Text.NdfFile.FamilyDef.ProcessCode (ndfLinesCount) as PC
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr)
import Noodle.Text.NdfFile.FamilyDef.Codegen as CodeGen
import Noodle.Ui.Cli.Tagging as F


newtype FamilyDef = FamilyDef FamilyDefRec
derive instance Newtype FamilyDef _
derive newtype instance Eq FamilyDef


newtype NodeFnDef = NodeFnDef (Fn ChannelDef ChannelDef)
derive instance Newtype NodeFnDef _
derive newtype instance Eq NodeFnDef


newtype ProcessAssign =
    ProcessAssign (FamilyR /\ ProcessCode)
derive instance Newtype ProcessAssign _
derive newtype instance Eq ProcessAssign


instance ToFn a ChannelDef ChannelDef FamilyDef where
    toFn :: Proxy a -> FamilyDef -> Fn ChannelDef ChannelDef
    toFn _ = _.fn <<< unwrap


instance ToFn a ChannelDef ChannelDef NodeFnDef where
    toFn :: Proxy a -> NodeFnDef -> Fn ChannelDef ChannelDef
    toFn = const unwrap


instance ToCode NDF opts NodeFnDef where
    toCode :: Proxy NDF -> opts -> NodeFnDef -> String
    toCode _ _ =
        case _ of
            NodeFnDef fn ->
                case (unwrap $ toFn (Proxy :: _ Void) fn :: FnS ChannelDef ChannelDef) of
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
                case (unwrap $ toFn (Proxy :: _ Void) fn :: FnS ChannelDef ChannelDef) of
                    (_ /\ inlets /\ outlets) ->
                        inletsList inlets <>
                        T.space <> F.operator "=>" <> T.space <>
                        case outlets of
                            [ singleOutput ] -> outletDefToTaggedCode singleOutput
                            _ -> outletsList outlets
        where
            inletsList inlets = F.operator "<" <> T.joinWith (T.space <> F.operator "->" <> T.space) (inletDefToTaggedCode <$> inlets) <> F.operator ">"
            outletsList outlets = F.operator "<" <> T.joinWith (T.space <> F.operator "->" <> T.space) (outletDefToTaggedCode <$> outlets) <> F.operator ">"
            inletDefToTaggedCode :: Argument ChannelDef -> T.Tag
            inletDefToTaggedCode arg =
                channelToTaggedCode_ F.inletId (argName arg) (argValue arg)
            outletDefToTaggedCode :: Output ChannelDef -> T.Tag
            outletDefToTaggedCode out =
                channelToTaggedCode_ F.outletId (outName out) (outValue out)


instance ToCode NDF opts FamilyDef where
    toCode :: Proxy NDF -> opts -> FamilyDef -> String
    toCode pndf opts (FamilyDef fdef) =
        case fdef of
            { group, fn, process, state } ->
                ": " <> Id.group group <> " : " <> (Fn.name fn) <> " :: "
                     <> (if hasStateDef_ state then stateToCode_ state <> " " else "")
                     <> toCode pndf opts (wrap fn :: NodeFnDef) <> case process of
                        NoneSpecified -> ""
                        _ -> " " <> toCode pndf opts process


instance ToTaggedCode NDF opts FamilyDef where
    toTaggedCode :: Proxy NDF -> opts -> FamilyDef -> T.Tag
    toTaggedCode pndf opts (FamilyDef fdef) =
        case fdef of
            { group, fn, process, state } ->
                F.operator ":"
                <> T.space <> F.someGroup (Id.group group) <> T.space <> F.operator ":"
                <> T.space <> F.family (Fn.name fn) <> T.space <> F.operator "::"
                <> (if hasStateDef_ state then T.space <> stateToTaggedCode_ state else T.space)
                <> toTaggedCode pndf opts (wrap fn :: NodeFnDef)
                <> case process of
                    NoneSpecified -> T.s ""
                    _ -> T.space <> toTaggedCode pndf opts process


instance ToCode NDF opts ProcessAssign where
    toCode :: Proxy NDF -> opts -> ProcessAssign -> String
    toCode pndf opts (ProcessAssign padef) =
        case padef of
            family /\ process ->
                "$ " <> Id.family family <> " :: " <> toCode pndf opts process


instance ToTaggedCode NDF opts ProcessAssign where
    toTaggedCode :: Proxy NDF -> opts -> ProcessAssign -> T.Tag
    toTaggedCode pndf opts (ProcessAssign padef) =
        case padef of
            family /\ process ->
                F.operator ":"
                <> T.space <> F.family (Id.family family) <> T.space <> F.operator "::"
                <> T.space <> toTaggedCode pndf opts process


instance (CodegenRepr strepr, CodegenRepr chrepr) => ToCode PS (CodeGen.Options strepr chrepr) FamilyDef where
    toCode :: Proxy PS -> CodeGen.Options strepr chrepr -> FamilyDef -> String
    toCode _ opts (FamilyDef fdef) =
        CodeGen.generate opts Nothing fdef


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
            F.operator "[" <> F.type_ typeStr <> T.space <> F.operator "{" <> F.value valueStr <> F.operator "}" <> F.operator "]"
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
            nameToTag chName <> F.operator ":" <> F.type_ typeStr <> T.space <> F.operator "{" <> F.value valueStr <> F.operator "}"
        Just (EncodedType typeStr) /\ Nothing ->
            nameToTag chName <> F.operator ":" <> F.type_ typeStr
        Nothing /\ Just (EncodedValue valueStr) ->
            nameToTag chName <> F.operator ":" <> F.operator "{" <> F.value valueStr <> F.operator "}"
        Nothing /\ Nothing ->
            nameToTag chName


group :: FamilyDef -> GroupR
group = unwrap >>> _.group


fnDef:: FamilyDef -> NodeFnDef
fnDef = unwrap >>> _.fn >>> wrap


family :: FamilyDef -> FamilyR
family = unwrap >>> _.fn >>> Fn.name >>> unsafeFamilyR


-- familyR :: FamilyDef -> Id.FamilyR
-- familyR = family >>> unwrap >>> Id.unsafeFamilyR


stateDef :: FamilyDef -> StateDef
stateDef = unwrap >>> _.state


processCode :: FamilyDef -> ProcessCode
processCode = unwrap >>> _.process


assignWhenNoneSpecified :: ProcessCode -> FamilyDef -> FamilyDef
assignWhenNoneSpecified pc familyDef =
    case pc of
        NoneSpecified -> familyDef # forceAssign pc
        _ -> familyDef


forceAssign :: ProcessCode -> FamilyDef -> FamilyDef
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


qdef :: { group :: String, family :: String, inputs :: Array (Argument ChannelDef), outputs :: Array (Output ChannelDef) } -> FamilyDef
qdef { group, family, inputs, outputs } =
    FamilyDef
        { group : unsafeGroupR group
        , fn : Make.fn family inputs outputs
        , state : emptyStateDef
        , process : NoneSpecified
        }


qdefp :: { group :: String, family :: String, inputs :: Array (Argument ChannelDef), outputs :: Array (Output ChannelDef), process :: ProcessCode } -> FamilyDef
qdefp { group, family, inputs, outputs, process } =
    FamilyDef
        { group : unsafeGroupR group
        , fn : Make.fn family inputs outputs
        , state : emptyStateDef
        , process
        }


qdefs :: { group :: String, family :: String, inputs :: Array (Argument ChannelDef), outputs :: Array (Output ChannelDef), state :: StateDef } -> FamilyDef
qdefs { group, family, inputs, outputs, state } =
    FamilyDef
        { group : unsafeGroupR group
        , fn : Make.fn family inputs outputs
        , state
        , process : NoneSpecified
        }


qdefps :: { group :: String, family :: String, inputs :: Array (Argument ChannelDef), outputs :: Array (Output ChannelDef), state :: StateDef, process :: ProcessCode } -> FamilyDef
qdefps { group, family, inputs, outputs, state, process } =
    FamilyDef
        { group : unsafeGroupR group
        , fn : Make.fn family inputs outputs
        , state
        , process
        }


qassign :: String -> ProcessCode -> ProcessAssign
qassign family pcode =
    wrap $ unsafeFamilyR family /\ pcode


ndfLinesCount :: FamilyDef -> Int
ndfLinesCount (FamilyDef { process }) =
    max 1 $ PC.ndfLinesCount process


processAssignNdfLinesCount :: ProcessAssign -> Int
processAssignNdfLinesCount (ProcessAssign (_ /\ process)) =
    max 1 $ PC.ndfLinesCount process