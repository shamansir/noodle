module Noodle.Text.Generators
    ( genSumType
    , genSeparateImports
    , genSeparateFamilyTypes
    , genSeparateFamilyImpls
    , genTypeDefSeparate
    , genToolkitModule
    , genToolkitDataModule
    , genFamilyModule
    , genTypeDefInline
    , genToolkitDef
    , genSepToolkitDef
    , moduleName, familyTypeName, typeConstructor
    )
    where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String as String
import Noodle.Text.QuickDef as QD


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


modulePrefix = "M"
typePrefix = "T"


i = "    "
i2 = i <> i
i3 = i2 <> i
i4 = i3 <> i
i5 = i4 <> i
i6 = i5 <> i


genSeparateImports :: String -> Array QD.QFamily -> String
genSeparateImports _ fmls =
    String.joinWith "\n" (genModuleImport <$> fmls)


-- TODO: Gen product type for Families


genFamilyModule :: String -> QD.QFamily -> String
genFamilyModule tkName qfml =
    "module " <> moduleName qfml <> " where\n\n"
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
    <> genFamilyTypeSepDef false qfml
    <> "\n\n"
    <> genFamilyToolkitSeparateImpl false qfml
    where
        inputProxyCode (Just ch) = "_in_" <> ch.name <> " = Fn.Input :: _ \"" <> ch.name <> "\""
        inputProxyCode Nothing = ""
        outputProxyCode (Just ch) = "_out_" <> ch.name <> " = Fn.Output :: _ \"" <> ch.name <> "\""
        outputProxyCode Nothing = ""


genToolkitModule :: String -> Array QD.QFamily -> String
genToolkitModule tkName families =
    "module Toolkit." <> ensureStartsFromCapitalLetter tkName <> "Gen where\n\n"
    <> genSeparateImports tkName families <> "\n\n\n"
    -- <> genSeparateFamilyTypes true tkName families
    <> genTypeDefSeparate true tkName families <> "\n\n\n"
    <> genSepToolkitDef tkName families


genToolkitDataModule :: String -> Array QD.QFamily -> String
genToolkitDataModule tkName families =
    "module " <> ensureStartsFromCapitalLetter tkName <> "GenData where\n\n\n"
    <> genSumType tkName families


genSumType :: String -> Array QD.QFamily -> String
genSumType tkName families =
    "data " <> ensureStartsFromCapitalLetter tkName <> "\n"
    <> i <> "= " <> String.joinWith ("\n" <> i <> "| ") (familyOption <$> families)
    where
        familyOption qfml =
            typeConstructor qfml
                <> if Array.length qfml.inputs > 0
                        then " " <> (String.joinWith " " (argType <$> qfml.inputs))
                        else ""
        argType Nothing = "?ArgType"
        argType (Just arg) = fromMaybe ("?" <> arg.name <> "_Type") arg.type


genTypeDefInline :: String -> Array QD.QFamily -> String
genTypeDefInline _ fmls =
    "type Toolkit m\n" <> i <> "= Toolkit Unit\n"
        <> i2 <> inBrackets' genFamilyTypeDef ("\n" <> i2 <> ", ") fmls


genSeparateFamilyTypes :: Boolean -> String -> Array QD.QFamily -> String
genSeparateFamilyTypes withModule _ fmls =
    String.joinWith "\n\n" (genFamilyTypeSepDef withModule <$> fmls) <> "\n\n"


genSeparateFamilyImpls :: Boolean -> String -> Array QD.QFamily -> String
genSeparateFamilyImpls withModule _ fmls =
    String.joinWith "\n\n" (genFamilyToolkitSeparateImpl withModule <$> fmls) <> "\n\n"


genTypeDefSeparate :: Boolean -> String -> Array QD.QFamily -> String
genTypeDefSeparate withModule _ fmls =
    "type Toolkit m\n" <> i <> "= Toolkit Unit\n"
        <> i2 <> inBrackets' (genFamilyTypeDefRef withModule) ("\n" <> i2 <> ", ") fmls


ensureStartsFromCapitalLetter :: String -> String
ensureStartsFromCapitalLetter str =
    case String.splitAt 1 str of
        { before, after } -> String.toUpper before <> after


typeConstructor :: QD.QFamily -> String
typeConstructor qfml =
    -- unwrap >>> _.family >>> moduleName'
    ensureStartsFromCapitalLetter qfml.family


moduleName :: QD.QFamily -> String
moduleName qfml =
    ensureStartsFromCapitalLetter qfml.tag <> "." <> modulePrefix <> ensureStartsFromCapitalLetter qfml.family


familyTypeName :: QD.QFamily -> String
familyTypeName qfml =
    typePrefix <> ensureStartsFromCapitalLetter qfml.family


genToolkitDef :: String -> Array QD.QFamily -> String
genToolkitDef tkName fmls =
    "toolkit =\n"
        <> i <> "Toolkit.from \"" <> tkName <> "\"\n"
        <> i2 <> inCBraces' genFamilyToolkitDef ("\n" <> i2 <> ", ") fmls


genSepToolkitDef :: String -> Array QD.QFamily -> String
genSepToolkitDef tkName fmls =
    "toolkit =\n"
        <> i <> "Toolkit.from \"" <> tkName <> "\"\n"
        <> i2 <> inCBraces' genFamilyToolkitDefRef ("\n" <> i2 <> ", ") fmls


genModuleImport :: QD.QFamily -> String
genModuleImport family =
   "import Toolkit.Families." <> moduleName family <> " as " <> moduleName family


genFamilyTypeDef :: QD.QFamily -> String
genFamilyTypeDef qfml =
    qfml.family <> " :: -- {-> " <> qfml.tag <> " <-}\n"
        <> i3 <> "Family.Def Unit\n"
        <> i4 <> (inBrackets genChanTypeDef ", " qfml.inputs) <> "\n"
        <> i4 <> (inBrackets genChanTypeDef ", " qfml.outputs) <> "\n"
        <> i4 <> "m"


genFamilyTypeDefRef :: Boolean -> QD.QFamily -> String
genFamilyTypeDefRef withModule qfml =
    qfml.family <> " :: "
        <> (if withModule then moduleName qfml <> ".Family" else "Family")
        <> " -- {-> " <> qfml.tag <> " <-}"


genFamilyTypeSepDef :: Boolean -> QD.QFamily -> String
genFamilyTypeSepDef withModule qfml =
    "type " <> (if withModule then moduleName qfml <> ".Family" else "Family")
        <> " m = -- {-> " <> qfml.tag <> " <-}\n"
        <> i <> "Family.Def Unit\n"
        <> i2 <> (inBrackets genChanTypeDef ", " qfml.inputs) <> "\n"
        <> i2 <> (inBrackets genChanTypeDef ", " qfml.outputs) <> "\n"
        <> i2 <> "m"


genFamilyToolkitDef :: QD.QFamily -> String
genFamilyToolkitDef qfml =
    qfml.family <> " : -- {-> " <> qfml.tag <> " <-}\n" <> moduleName qfml


genFamilyToolkitDefRef :: QD.QFamily -> String
genFamilyToolkitDefRef qfml =
    qfml.family <> " : " <> moduleName qfml <> ".family"


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
            "-- " <> typeConstructor qfml <>
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



genFamilyToolkitSeparateImpl :: Boolean -> QD.QFamily -> String
genFamilyToolkitSeparateImpl withModule qfml =
    if withModule then
        ( moduleName qfml <> ".family :: forall m. " <> moduleName qfml <> ".Family m\n"
        <> moduleName qfml <> ".family = -- {-> " <> qfml.tag <> " <-}\n"
        )
    else
        ( "family :: forall m. Family m\n"
        <> "family = -- {-> " <> qfml.tag <> " <-}\n"
        )
        <> i <> "Family.def\n"
        <> i2 <> "unit\n"
        <> i2 <> (inCBraces genChanToolkitDef ", " qfml.inputs) <> "\n"
        <> i2 <> (inCBraces genChanToolkitDef ", " qfml.outputs) <> "\n"
        <> i2 <> "$ Fn.make $ " <> processBody i3 qfml


genChanToolkitDef :: Maybe QD.Channel -> String
genChanToolkitDef (Just arg) =
    arg.name <> " : " <> (fromMaybe ("?" <> arg.name <> "_default") arg.default)
genChanToolkitDef Nothing =
    "?ch_default"


genChanTypeDef :: Maybe QD.Channel -> String
genChanTypeDef (Just arg) =
    arg.name <> " :: " <> (fromMaybe "Unknown" arg.type)
genChanTypeDef Nothing =
    "?ch_type"