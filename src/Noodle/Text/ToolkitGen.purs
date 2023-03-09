module Noodle.Text.ToolkitGen where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
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


inCBraces' :: forall a. (a → String) → String → Array a → String
inCBraces' =
    wrap "{ }" "{ " $ "\n" <> i2 <> "}"


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
genFamilyModule tkName (QD.QFamily fml) =
    "module " <> moduleName fml.family <> " where\n\n"
    <> "import Prelude\n\n"
    <> String.joinWith "\n" (inputProxyCode <$> fml.inputs)
    <> "\n\n"
    <> String.joinWith "\n" (outputProxyCode <$> fml.outputs)
    <> "\n\n"
    <> genFamilyTypeSepDef false (QD.QFamily fml)
    <> "\n\n"
    <> genFamilyToolkitSeparateImpl false (QD.QFamily fml)
    where
        inputProxyCode (Just ch) = "_in_" <> ch.name <> " = Fn.Input :: _ \"" <> ch.name <> "\""
        inputProxyCode Nothing = ""
        outputProxyCode (Just ch) = "_out_" <> ch.name <> " = Fn.Output :: _ \"" <> ch.name <> "\""
        outputProxyCode Nothing = ""


genSumType :: String -> Array QD.QFamily -> String
genSumType tkName families =
    "data " <> ensureStartsFromCapitalLetter tkName <> "\n"
    <> i <> "= " <> String.joinWith ("\n" <> i <> "| ") (familyOption <$> families)
    where
        familyOption (QD.QFamily fml) =
            ensureStartsFromCapitalLetter fml.family
                <> if Array.length fml.inputs > 0
                        then " " <> (String.joinWith " " (argType <$> fml.inputs))
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


moduleName :: String -> String
moduleName family =
    "M" <> ensureStartsFromCapitalLetter family


familyTypeName :: String -> String
familyTypeName family =
    "T" <> ensureStartsFromCapitalLetter family


genToolkitDef :: String -> Array QD.QFamily -> String
genToolkitDef tkName fmls =
    "toolkit =\n"
        <> i <> "Toolkit.from \"" <> tkName <> "\"\n"
        <> i2 <> inCBraces' genFamilyToolkitDef ("\n" <> i2 <> ", ") fmls


genModuleImport :: QD.QFamily -> String
genModuleImport (QD.QFamily fml) =
   "import Toolkit.Families." <> moduleName fml.family <> " as " <> moduleName fml.family


genFamilyTypeDef :: QD.QFamily -> String
genFamilyTypeDef (QD.QFamily fml) =
    fml.family <> " :: -- {-> " <> fml.tag <> " <-}\n"
        <> i3 <> "Family.Def Unit\n"
        <> i4 <> (inBrackets genChanTypeDef ", " fml.inputs) <> "\n"
        <> i4 <> (inBrackets genChanTypeDef ", " fml.outputs) <> "\n"
        <> i4 <> "m"


genFamilyTypeDefRef :: Boolean -> QD.QFamily -> String
genFamilyTypeDefRef withModule (QD.QFamily fml) =
    fml.family <> " :: "
        <> (if withModule then moduleName fml.family <> ".Family" else "Family")
        <> " -- {-> " <> fml.tag <> " <-}"


genFamilyTypeSepDef :: Boolean -> QD.QFamily -> String
genFamilyTypeSepDef withModule (QD.QFamily fml) =
    "type " <> (if withModule then moduleName fml.family <> ".Family" else "Family")
        <> " m = -- {-> " <> fml.tag <> " <-}\n"
        <> i <> "Family.Def Unit\n"
        <> i2 <> (inBrackets genChanTypeDef ", " fml.inputs) <> "\n"
        <> i2 <> (inBrackets genChanTypeDef ", " fml.outputs) <> "\n"
        <> i2 <> "m"


genFamilyToolkitDef :: QD.QFamily -> String
genFamilyToolkitDef (QD.QFamily fml) =
    fml.family <> " : -- {-> " <> fml.tag <> " <-}\n"
        <> i3 <> "Family.def\n"
        <> i4 <> "unit\n"
        <> i4 <> (inCBraces genChanToolkitDef ", " fml.inputs) <> "\n"
        <> i4 <> (inCBraces genChanToolkitDef ", " fml.outputs) <> "\n"
        <> i4 <> "$ fml.make $ " <> processBody i5 (QD.QFamily fml)


processBody :: String -> QD.QFamily -> String
processBody indent (QD.QFamily fml) =
    if (Array.length fml.inputs > 0) || (Array.length fml.outputs > 0) then
        ("do\n"
            <> (if Array.length fml.inputs > 0 then
                    (indent <> String.joinWith ("\n" <> indent) (inputReceive <$> fml.inputs) <> "\n")
                else "")
            <> (if Array.length fml.outputs > 0 then
                    (indent <> String.joinWith ("\n" <> indent) (outputSend <$> fml.outputs) <> "\n")
                else (indent <> "pure unit\n"))
        )
    else (indent <> "pure unit\n")
    where
        inputReceive Nothing = "--"
        inputReceive (Just ch) = ch.name <> " <- P.receive _in_" <> ch.name
        outputSend Nothing = "--"
        outputSend (Just ch) = "P.send _out_" <> ch.name <> " ?out_" <> ch.name



genFamilyToolkitSeparateImpl :: Boolean -> QD.QFamily -> String
genFamilyToolkitSeparateImpl withModule (QD.QFamily fml) =
    if withModule then
        ( moduleName fml.family <> ".fml :: forall m. " <> moduleName fml.family <> ".Family m\n"
        <> moduleName fml.family <> ".fml = -- {-> " <> fml.tag <> " <-}\n"
        )
    else
        ( "fml :: forall m. Family m\n"
        <> "fml = -- {-> " <> fml.tag <> " <-}\n"
        )
        <> i <> "Family.def\n"
        <> i2 <> "unit\n"
        <> i2 <> (inCBraces genChanToolkitDef ", " fml.inputs) <> "\n"
        <> i2 <> (inCBraces genChanToolkitDef ", " fml.outputs) <> "\n"
        <> i2 <> "$ fml.make $ " <> processBody i3 (QD.QFamily fml)


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