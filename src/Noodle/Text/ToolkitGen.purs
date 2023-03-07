module Noodle.Text.ToolkitGen where

import Prelude

import Data.Array as Array
import Data.String as String
import Data.Maybe (Maybe(..), fromMaybe)

import Noodle.Text.QuickDef as QD


wrap :: forall a. String -> String → String → (a → String) → String → Array a → String
wrap empty start end fn sep items =
    if Array.length items == 0 then empty
    else start <> String.joinWith sep (fn <$> items) <> end


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


genSeparateImports :: String -> Array QD.QFamily -> String
genSeparateImports _ fns =
    String.joinWith "\n" (genModuleImport <$> fns)


genTypeDefInline :: String -> Array QD.QFamily -> String
genTypeDefInline _ fns =
    "type Toolkit m\n" <> i <> "= Toolkit Unit\n"
        <> i2 <> inBrackets' genFamilyTypeDef ("\n" <> i2 <> ", ") fns


genSeparateFamilyTypes :: Boolean -> String -> Array QD.QFamily -> String
genSeparateFamilyTypes withModule _ fns =
    String.joinWith "\n\n" (genFamilyTypeSepDef withModule <$> fns) <> "\n\n"


genSeparateFamilyImpls :: String -> Array QD.QFamily -> String
genSeparateFamilyImpls _ fns =
    String.joinWith "\n\n" (genFamilyToolkitSeparateImpl <$> fns) <> "\n\n"


genTypeDefSeparate :: Boolean -> String -> Array QD.QFamily -> String
genTypeDefSeparate withModule _ fns =
    "type Toolkit m\n" <> i <> "= Toolkit Unit\n"
        <> i2 <> inBrackets' (genFamilyTypeDefRef withModule) ("\n" <> i2 <> ", ") fns


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
genToolkitDef tkName fns =
    "toolkit =\n"
        <> i <> "Toolkit.from \"" <> tkName <> "\"\n"
        <> i2 <> inCBraces' genFamilyToolkitDef ("\n" <> i2 <> ", ") fns


genModuleImport :: QD.QFamily -> String
genModuleImport (QD.QFamily fn) =
   "import Toolkit.Families." <> moduleName fn.family <> " as " <> moduleName fn.family


genFamilyTypeDef :: QD.QFamily -> String
genFamilyTypeDef (QD.QFamily fn) =
    fn.family <> " :: -- {-> " <> fn.tag <> " <-}\n"
        <> i3 <> "Family.Def Unit\n"
        <> i4 <> (inBrackets genChanTypeDef ", " fn.inputs) <> "\n"
        <> i4 <> (inBrackets genChanTypeDef ", " fn.outputs) <> "\n"
        <> i4 <> "m"


genFamilyTypeDefRef :: Boolean -> QD.QFamily -> String
genFamilyTypeDefRef withModule (QD.QFamily fn) =
    fn.family <> " :: "
        <> (if withModule then moduleName fn.family <> ".Family" else "Family")
        <> " -- {-> " <> fn.tag <> " <-}"


genFamilyTypeSepDef :: Boolean -> QD.QFamily -> String
genFamilyTypeSepDef withModule (QD.QFamily fn) =
    "type " <> (if withModule then moduleName fn.family <> ".Family" else "Family")
        <> " m = -- {-> " <> fn.tag <> " <-}\n"
        <> i <> "Family.Def Unit\n"
        <> i2 <> (inBrackets genChanTypeDef ", " fn.inputs) <> "\n"
        <> i2 <> (inBrackets genChanTypeDef ", " fn.outputs) <> "\n"
        <> i2 <> "m"


genFamilyToolkitDef :: QD.QFamily -> String
genFamilyToolkitDef (QD.QFamily fn) =
    fn.family <> " : -- {-> " <> fn.tag <> " <-}\n"
        <> i3 <> "Family.def\n"
        <> i4 <> "unit\n"
        <> i4 <> (inCBraces genChanToolkitDef ", " fn.inputs) <> "\n"
        <> i4 <> (inCBraces genChanToolkitDef ", " fn.outputs) <> "\n"
        <> i4 <> "$ Fn.make $ pure unit"


genFamilyToolkitSeparateImpl :: QD.QFamily -> String
genFamilyToolkitSeparateImpl (QD.QFamily fn) =
    moduleName fn.family <> ".fn :: forall m. " <> moduleName fn.family <> ".Family m\n"
    <> moduleName fn.family <> ".fn = -- {-> " <> fn.tag <> " <-}\n"
        <> i <> "Family.def\n"
        <> i2 <> "unit\n"
        <> i2 <> (inCBraces genChanToolkitDef ", " fn.inputs) <> "\n"
        <> i2 <> (inCBraces genChanToolkitDef ", " fn.outputs) <> "\n"
        <> i <> "$ Fn.make $ pure unit"


genChanToolkitDef :: Maybe QD.Channel -> String
genChanToolkitDef (Just arg) =
    arg.name <> " : " <> (fromMaybe "?" arg.default)
genChanToolkitDef Nothing =
    "?"


genChanTypeDef :: Maybe QD.Channel -> String
genChanTypeDef (Just arg) =
    arg.name <> " :: " <> (fromMaybe "Unknown" arg.type)
genChanTypeDef Nothing =
    "?"