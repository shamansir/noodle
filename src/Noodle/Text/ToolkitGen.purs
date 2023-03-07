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


genSeparateImports :: String -> Array QD.FN -> String
genSeparateImports _ fns =
    String.joinWith "\n" (genModuleImport <$> fns)


genTypeDefInline :: String -> Array QD.FN -> String
genTypeDefInline _ fns =
    "type Toolkit m\n" <> i <> "= Toolkit Unit\n"
        <> i2 <> inBrackets' genFamilyTypeDef ("\n" <> i2 <> ", ") fns


genSeparateFamilyTypes :: String -> Array QD.FN -> String
genSeparateFamilyTypes _ fns =
    String.joinWith "\n\n" (genFamilyTypeSepDef <$> fns) <> "\n\n"


genSeparateFamilyImpls :: String -> Array QD.FN -> String
genSeparateFamilyImpls _ fns =
    String.joinWith "\n\n" (genFamilyToolkitSeparateImpl <$> fns) <> "\n\n"


genTypeDefSeparate :: String -> Array QD.FN -> String
genTypeDefSeparate _ fns =
    "type Toolkit m\n" <> i <> "= Toolkit Unit\n"
        <> i2 <> inBrackets' genFamilyTypeDefRef ("\n" <> i2 <> ", ") fns


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


genToolkitDef :: String -> Array QD.FN -> String
genToolkitDef tkName fns =
    "toolkit =\n"
        <> i <> "Toolkit.from \"" <> tkName <> "\"\n"
        <> i2 <> inCBraces' genFamilyToolkitDef ("\n" <> i2 <> ", ") fns


genModuleImport :: QD.FN -> String
genModuleImport (QD.FN fn) =
   "import Toolkit.Families." <> moduleName fn.family <> " as " <> moduleName fn.family


genFamilyTypeDef :: QD.FN -> String
genFamilyTypeDef (QD.FN fn) =
    fn.family <> " :: -- {-> " <> fn.tag <> " <-}\n"
        <> i3 <> "Family.Def Unit\n"
        <> i4 <> (inBrackets genArgToolkitDef ", " fn.args) <> "\n"
        <> i4 <> "( out :: " <> fn.returns <> " )\n"
        <> i4 <> "m"


genFamilyTypeDefRef :: QD.FN -> String
genFamilyTypeDefRef (QD.FN fn) =
    fn.family <> " :: "
        <> moduleName fn.family <> ".Family"
        <> " -- {-> " <> fn.tag <> " <-}"


genFamilyTypeSepDef :: QD.FN -> String
genFamilyTypeSepDef (QD.FN fn) =
    "type " <> moduleName fn.family <> ".Family" <> " m = -- {-> " <> fn.tag <> " <-}\n"
        <> i <> "Family.Def Unit\n"
        <> i2 <> (inBrackets genArgToolkitDef ", " fn.args) <> "\n"
        <> i2 <> "( out :: " <> fn.returns <> " )\n"
        <> i2 <> "m"


genFamilyToolkitDef :: QD.FN -> String
genFamilyToolkitDef (QD.FN fn) =
    fn.family <> " : -- {-> " <> fn.tag <> " <-}\n"
        <> i3 <> "Family.def\n"
        <> i4 <> "unit\n"
        <> i4 <> (inCBraces genArgToolkitDef ", " fn.args) <> "\n"
        <> i4 <> "{ out : " <> fn.returns <> " }\n"
        <> i4 <> "$ Fn.make $ pure unit"


genFamilyToolkitSeparateImpl :: QD.FN -> String
genFamilyToolkitSeparateImpl (QD.FN fn) =
    moduleName fn.family <> ".fn :: forall m. " <> moduleName fn.family <> ".Family m\n"
    <> moduleName fn.family <> ".fn = -- {-> " <> fn.tag <> " <-}\n"
        <> i <> "Family.def\n"
        <> i2 <> "unit\n"
        <> i2 <> (inCBraces genArgToolkitDef ", " fn.args) <> "\n"
        <> i2 <> "{ out : " <> fn.returns <> " }\n"
        <> i <> "$ Fn.make $ pure unit"


genArgToolkitDef :: Maybe QD.Argument -> String
genArgToolkitDef (Just arg) =
    arg.name <> " : " <> (fromMaybe "?" arg.default)
genArgToolkitDef Nothing =
    "?"


genArgTypeDef :: Maybe QD.Argument -> String
genArgTypeDef (Just arg) =
    arg.name <> " :: " <> (fromMaybe "Unknown" arg.type)
genArgTypeDef Nothing =
    "?"