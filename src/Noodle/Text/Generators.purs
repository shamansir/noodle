module Noodle.Text.Generators
    ( toolkitSumType
    , toolkitDataModule
    , toolkitType
    , toolkitTypeInline
    , toolkitModule
    , toolkitImplementation
    , toolkitImplementationInline

    , importAllFamilies
    , familiesTypes
    , familiesImplementations
    , familyModule

    , familyModuleName, familyTypeName, familyTypeConstructor

    , ToolkitName(..)
    )
    where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, class Newtype)
import Data.String as String
import Noodle.Text.QuickDef as QD


newtype ToolkitName = ToolkitName String

derive instance Newtype ToolkitName _

wrap :: forall a. String -> String → String → (a → String) → String → Array a → String
wrap empty start end fml sep items =
    if Array.length items == 0 then empty
    else start <> String.joinWith sep (fml <$> items) <> end


inBrackets :: forall a. (a → String) → String → Array a → String
inBrackets =
    wrap "( )" "( " " )"


inBrackets' :: forall a. (a → String) → String → Array a → String
inBrackets' =
    wrap "( )" "( " $ "\n" <> i2 <> ")"


inCBraces :: forall a. (a → String) → String → Array a → String
inCBraces =
    wrap "{ }" "{ " " }"


inCBraces' :: forall a. (a -> String) -> String -> Array a -> String
inCBraces' =
    wrap "{ }" "{ " $ "\n" <> i2 <> "}"


ensureStartsFromCapitalLetter :: String -> String
ensureStartsFromCapitalLetter str =
    case String.splitAt 1 str of
        { before, after } -> String.toUpper before <> after


modulePrefix = "M"
typePrefix = "T"


i = "    "
i2 = i <> i
i3 = i2 <> i
i4 = i3 <> i
i5 = i4 <> i
i6 = i5 <> i


familyModule :: ToolkitName -> QD.QFamily -> String
familyModule _ qfml =
    "module " <> familyModuleName qfml <> " where\n\n"
    <> "import Prelude\n"
    <> "\n"
    <> "import Noodle.Fn2 as Fn\n"
    <> "import Noodle.Fn2.Process as P\n"
    <> "import Noodle.Family.Def as Family\n"
    <> "\n"
    <> String.joinWith "\n" (inputProxyCode <$> qfml.inputs)
    <> "\n\n"
    <> String.joinWith "\n" (outputProxyCode <$> qfml.outputs)
    <> "\n\n"
    <> familyType qfml
    <> "\n\n"
    <> familyImplementation qfml
    where
        inputProxyCode (Just ch) = "_in_" <> ch.name <> " = Fn.Input :: _ \"" <> ch.name <> "\""
        inputProxyCode Nothing = ""
        outputProxyCode (Just ch) = "_out_" <> ch.name <> " = Fn.Output :: _ \"" <> ch.name <> "\""
        outputProxyCode Nothing = ""


toolkitModule :: ToolkitName -> Array QD.QFamily -> String
toolkitModule tkName families =
    "module Toolkit." <> ensureStartsFromCapitalLetter (unwrap tkName) <> "Gen where\n\n"
    <> importAllFamilies families <> "\n\n\n"
    -- <> familiesTypes true tkName families
    <> toolkitType families <> "\n\n\n"
    <> toolkitImplementation tkName families


toolkitDataModule :: ToolkitName -> Array QD.QFamily -> String
toolkitDataModule tkName families =
    "module " <> ensureStartsFromCapitalLetter (unwrap tkName) <> "GenData where\n\n\n"
    <> toolkitSumType tkName families


toolkitSumType :: ToolkitName -> Array QD.QFamily -> String
toolkitSumType tkName families =
    "data " <> ensureStartsFromCapitalLetter (unwrap tkName) <> "\n"
    <> i <> "= " <> String.joinWith ("\n" <> i <> "| ") (familyOption <$> families)
    where
        familyOption qfml =
            familyTypeConstructor qfml
                <> if Array.length qfml.inputs > 0
                        then " " <> (String.joinWith " " (argType <$> qfml.inputs))
                        else ""
        argType Nothing = "?ArgType"
        argType (Just arg) = fromMaybe ("?" <> arg.name <> "_Type") arg.type


toolkitTypeInline :: ToolkitName -> Array QD.QFamily -> String
toolkitTypeInline _ fmls =
    "type Toolkit m\n" <> i <> "= Toolkit Unit\n"
        <> i2 <> inBrackets' familyTypeInline ("\n" <> i2 <> ", ") fmls


toolkitType :: Array QD.QFamily -> String
toolkitType fmls =
    "type Toolkit m\n" <> i <> "= Toolkit Unit\n"
        <> i2 <> inBrackets' familyTypeReference ("\n" <> i2 <> ", ") fmls


toolkitImplementationInline :: ToolkitName -> Array QD.QFamily -> String
toolkitImplementationInline tkName fmls =
    "toolkit =\n"
        <> i <> "Toolkit.from \"" <> unwrap tkName <> "\"\n"
        <> i2 <> inCBraces' familyInlineImplementationReferenceAndLabel ("\n" <> i2 <> ", ") fmls


toolkitImplementation :: ToolkitName -> Array QD.QFamily -> String
toolkitImplementation tkName fmls =
    "toolkit =\n"
        <> i <> "Toolkit.from \"" <> unwrap tkName <> "\"\n"
        <> i2 <> inCBraces' familyModuleImplementationReferenceAndLabel ("\n" <> i2 <> ", ") fmls


importAllFamilies :: Array QD.QFamily -> String
importAllFamilies fmls =
    String.joinWith "\n" (familyModuleImport <$> fmls)


familiesTypes :: Array QD.QFamily -> String
familiesTypes fmls =
    String.joinWith "\n\n" (familyType <$> fmls) <> "\n\n"


familiesImplementations :: Array QD.QFamily -> String
familiesImplementations fmls =
    String.joinWith "\n\n" (familyImplementation <$> fmls) <> "\n\n"


familyTypeConstructor :: QD.QFamily -> String
familyTypeConstructor qfml =
    -- unwrap >>> _.family >>> familyModuleName'
    ensureStartsFromCapitalLetter qfml.family


familyModuleName :: QD.QFamily -> String
familyModuleName qfml =
    ensureStartsFromCapitalLetter qfml.tag <> "." <> modulePrefix <> ensureStartsFromCapitalLetter qfml.family


familyTypeName :: QD.QFamily -> String
familyTypeName qfml =
    typePrefix <> ensureStartsFromCapitalLetter qfml.family


familyModuleImport :: QD.QFamily -> String
familyModuleImport family =
   "import Toolkit.Families." <> familyModuleName family <> " as " <> familyModuleName family


familyTypeInline :: QD.QFamily -> String
familyTypeInline qfml =
    qfml.family <> " :: -- {-> " <> qfml.tag <> " <-}\n"
        <> i3 <> "Family.Def Unit\n"
        <> i4 <> (inBrackets channelTypeAndLabel ", " qfml.inputs) <> "\n"
        <> i4 <> (inBrackets channelTypeAndLabel ", " qfml.outputs) <> "\n"
        <> i4 <> "m"



familyType :: QD.QFamily -> String
familyType qfml =
    "type Family"
        <> " m = -- {-> " <> qfml.tag <> " <-}\n"
        <> i <> "Family.Def Unit\n"
        <> i2 <> (inBrackets channelTypeAndLabel ", " qfml.inputs) <> "\n"
        <> i2 <> (inBrackets channelTypeAndLabel ", " qfml.outputs) <> "\n"
        <> i2 <> "m"


familyTypeReference :: QD.QFamily -> String
familyTypeReference qfml =
    qfml.family <> " :: "
        <> familyModuleName qfml <> ".Family"
        <> " -- {-> " <> qfml.tag <> " <-}"


familyInlineImplementationReferenceAndLabel :: QD.QFamily -> String
familyInlineImplementationReferenceAndLabel qfml =
    qfml.family <> " : -- {-> " <> qfml.tag <> " <-}\n" <> familyModuleName qfml


familyModuleImplementationReferenceAndLabel :: QD.QFamily -> String
familyModuleImplementationReferenceAndLabel qfml =
    qfml.family <> " : " <> familyModuleName qfml <> ".family"


processBody :: String -> QD.QFamily -> String
processBody indent qfml =
    if (Array.length qfml.inputs > 0) || (Array.length qfml.outputs > 0) then
        ("do\n"
            <> (if Array.length qfml.inputs > 0 then
                    (indent <> String.joinWith ("\n" <> indent) (inputReceive <$> qfml.inputs) <> "\n")
                else "")
            <> indent <> outputComment <> "\n"
            <> (if Array.length qfml.outputs > 0 then
                    (indent <> String.joinWith ("\n" <> indent) (outputSend <$> qfml.outputs) <> "\n")
                else (indent <> "pure unit\n"))
        )
    else (indent <> outputComment <> "\n" <> indent <> "pure unit\n")
    where
        outputComment =
            "-- " <> familyTypeConstructor qfml <>
                (if (Array.length qfml.inputs > 0)
                    then " " <> String.joinWith " " (inputVarName <$> qfml.inputs)
                    else ""
                )
        inputVarName Nothing = "?input"
        inputVarName (Just ch) = ch.name -- "_in_" <> ch.name
        inputReceive Nothing = "--"
        inputReceive (Just ch) = ch.name <> " <- P.receive _in_" <> ch.name
        outputSend Nothing = "--"
        outputSend (Just ch) = "P.send _out_" <> ch.name <> " ?out_" <> ch.name



familyImplementation :: QD.QFamily -> String
familyImplementation qfml =
    "family :: forall m. Family m\n"
    <> "family = -- {-> " <> qfml.tag <> " <-}\n"
    <> i <> "Family.def\n"
    <> i2 <> "unit\n"
    <> i2 <> (inCBraces channelDefaultAndLabel ", " qfml.inputs) <> "\n"
    <> i2 <> (inCBraces channelDefaultAndLabel ", " qfml.outputs) <> "\n"
    <> i2 <> "$ Fn.make $ " <> processBody i3 qfml


channelTypeAndLabel :: Maybe QD.Channel -> String
channelTypeAndLabel (Just ch) =
    ch.name <> " :: " <> (fromMaybe "Unknown" ch.type)
channelTypeAndLabel Nothing =
    "?ch_type"


channelDefaultAndLabel :: Maybe QD.Channel -> String
channelDefaultAndLabel (Just ch) =
    ch.name <> " : " <> (fromMaybe ("?" <> ch.name <> "_default") ch.default)
channelDefaultAndLabel Nothing =
    "?ch_default"