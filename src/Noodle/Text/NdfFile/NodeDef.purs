module Noodle.Text.NdfFile.NodeDef where

import Prelude

import Type.Proxy (Proxy)

import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Text.Format as T

import Noodle.Text.ToCode (class ToCode, toCode, class ToTaggedCode, toTaggedCode, NDF)
import Noodle.Fn.ToFn (fn, Fn, toFn, Argument, Output, argName, argValue, outName, outValue, arg, out)
import Noodle.Text.NdfFile.Newtypes (EncodedType(..), EncodedValue(..), FamilyGroup, NodeFamily, ProcessCode)
import Noodle.Ui.Cli.Tagging as F


type ChannelDef = { dataType :: Maybe EncodedType, defaultValue :: Maybe EncodedValue }


newtype NodeDef = NodeDef
    { group :: FamilyGroup
    , family :: NodeFamily -- FIXME: family stored both in function definition and here
    , fn :: NodeFnDef
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


instance ToCode NDF NodeFnDef where
    toCode :: Proxy NDF -> NodeFnDef -> String
    toCode _ =
        case _ of
            NodeFnDef fn ->
                case (toFn fn :: String /\ Array (Argument ChannelDef) /\ Array (Output ChannelDef)) of
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
            inletDefToCode :: Argument ChannelDef -> String
            inletDefToCode arg =
                channelToCode_ (argName arg) (argValue arg)
            outletDefToCode :: Output ChannelDef -> String
            outletDefToCode out =
                channelToCode_ (outName out) (outValue out)


instance ToTaggedCode NDF NodeFnDef where
    toTaggedCode :: Proxy NDF -> NodeFnDef -> T.Tag
    toTaggedCode _ =
        case _ of
            NodeFnDef fn ->
                case (toFn fn :: String /\ Array (Argument ChannelDef) /\ Array (Output ChannelDef)) of
                    (_ /\ inlets /\ outlets) ->
                        inletsList inlets <>
                        T.s " " <> F.operator "=>" <> T.s " " <>
                        case outlets of
                            [ singleOutput ] ->
                                if outName singleOutput == "out" then outletDefToTaggedCode singleOutput else outletsList outlets
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


instance ToCode NDF NodeDef where
    toCode :: Proxy NDF -> NodeDef -> String
    toCode pndf (NodeDef ndef) =
        case ndef of
            { group, family, fn, process } ->
                ": " <> unwrap group <> " : " <> unwrap family <> " :: " <> toCode pndf fn <> case process of
                    Just processCode -> " /-|" <> unwrap processCode <> "|-/"
                    Nothing -> ""


instance ToTaggedCode NDF NodeDef where
    toTaggedCode :: Proxy NDF -> NodeDef -> T.Tag
    toTaggedCode pndf (NodeDef ndef) =
        case ndef of
            { group, family, fn, process } ->
                F.operator ":"
                <> T.s " " <> F.someGroup (unwrap group) <> T.s " " <> F.operator ":"
                <> T.s " " <> F.family (unwrap family) <> T.s " " <> F.operator "::"
                <> T.s " " <> toTaggedCode pndf fn
                <> case process of
                    Just processCode -> T.s " " <> F.operator "/-|" <> T.s (unwrap processCode) <> F.operator "|-/"
                    Nothing -> T.s ""


instance ToCode NDF ProcessAssign where
    toCode :: Proxy NDF -> ProcessAssign -> String
    toCode _ (ProcessAssign padef) =
        case padef of
            family /\ process ->
                "$ " <> unwrap family <> " :: " <> "/-|" <> unwrap process <> "|-/"


instance ToTaggedCode NDF ProcessAssign where
    toTaggedCode :: Proxy NDF -> ProcessAssign -> T.Tag
    toTaggedCode _ (ProcessAssign padef) =
        case padef of
            family /\ process ->
                F.operator ":"
                <> T.s " " <> F.family (unwrap family) <> T.s " " <> F.operator "::"
                <> T.s " " <> F.operator "/-|" <> T.s (unwrap process) <> F.operator "|-/"


channelToCode_ :: String -> ChannelDef -> String
channelToCode_ chName { dataType, defaultValue } =
    case (dataType /\ defaultValue) of
        Just (EncodedType dataTypeStr) /\ Just (EncodedValue valueStr) ->
            chName <> ":" <> dataTypeStr <> " " <> "{" <> valueStr <> "}"
        Just (EncodedType dataTypeStr) /\ Nothing ->
            chName <> ":" <> dataTypeStr
        Nothing /\ Just (EncodedValue valueStr) ->
            chName <> ":" <> "{" <> valueStr <> "}"
        Nothing /\ Nothing ->
            chName


channelToTaggedCode_ :: (String -> T.Tag) -> String -> ChannelDef -> T.Tag
channelToTaggedCode_ nameToTag chName { dataType, defaultValue } =
    case (dataType /\ defaultValue) of
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
i { name, type_, value } = channelToArg name { dataType : EncodedType <$> type_, defaultValue : EncodedValue <$> value }


o :: { name :: String, type_ :: Maybe String, value :: Maybe String } -> Output ChannelDef
o { name, type_, value } = channelToOut name { dataType : EncodedType <$> type_, defaultValue : EncodedValue <$> value }


ch :: String -> { name :: String, type_ :: Maybe String, value :: Maybe String }
ch name = { name, type_ : Nothing, value : Nothing }


chtv :: String -> String -> String -> { name :: String, type_ :: Maybe String, value :: Maybe String }
chtv name type_ value = { name, type_ : Just type_, value : Just value }


cht :: String -> String -> { name :: String, type_ :: Maybe String, value :: Maybe String }
cht name type_ = { name, type_ : Just type_, value : Nothing }


chv :: String -> String -> { name :: String, type_ :: Maybe String, value :: Maybe String }
chv name value = { name, type_ : Nothing, value : Just value }


qdef :: { group :: String, family :: String, inputs :: Array (Argument ChannelDef), outputs :: Array (Output ChannelDef) } -> NodeDef
qdef { group, family, inputs, outputs } =
    NodeDef
        { group : wrap group
        , family : wrap family
        , fn : wrap $ fn family inputs outputs
        , process : Nothing
        }


qdefp :: { group :: String, family :: String, inputs :: Array (Argument ChannelDef), outputs :: Array (Output ChannelDef), process :: String } -> NodeDef
qdefp { group, family, inputs, outputs, process } =
    NodeDef
        { group : wrap group
        , family : wrap family
        , fn : wrap $ fn family inputs outputs
        , process : Just $ wrap process
        }