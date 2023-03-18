module Noodle.Text.Generators
    ( toolkitSumType
    , toolkitDataModule
    , toolkitModule
    , familyModule
    , Way(..)
    , ToolkitName(..)
    , LocalsPrefix(..)

    , toolkitModulePath
    , toolkitDataModulePath
    , familyModulePath
    , familiesModulesDirectoryPath
    )
    where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, class Newtype)
import Data.String as String
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Text.QuickDef as QD


data Way
    = FamiliesInline
    | FamiliesAsModules


newtype ToolkitName = ToolkitName String

derive instance Newtype ToolkitName _


newtype LocalsPrefix = LocalsPrefix String

derive instance Newtype LocalsPrefix _



toolkitModulePath :: ToolkitName -> String
toolkitModulePath (ToolkitName tkName) = "/" <> ensureStartsFromCapitalLetter tkName <> ".purs"


toolkitDataModulePath :: ToolkitName -> String
toolkitDataModulePath (ToolkitName tkName) = "/" <> ensureStartsFromCapitalLetter tkName <> toolkitDataModuleNamePostfix <> ".purs"


familyModulePath :: QD.QFamily -> String
familyModulePath family = "/Family/" <> familyModuleFilename family <> ".purs"


familiesModulesDirectoryPath :: String
familiesModulesDirectoryPath = "/Family"


toolkitTypeName :: ToolkitName -> String
toolkitTypeName tkName =
    ensureStartsFromCapitalLetter (unwrap tkName) <> toolkitTypeNamePostfix


toolkitModuleName :: ToolkitName -> String
toolkitModuleName tkName =
    "Toolkit." <> ensureStartsFromCapitalLetter (unwrap tkName) <> toolkitModuleNamePostfix


toolkitDataModuleName :: ToolkitName -> String
toolkitDataModuleName tkName =
    "Toolkit." <> ensureStartsFromCapitalLetter (unwrap tkName) <> toolkitDataModuleNamePostfix


familyModuleFilename :: QD.QFamily -> String
familyModuleFilename qfml =
    case qfml.tag of
        Just familyTag ->
            ensureStartsFromCapitalLetter familyTag <> "." <> toolkitModulePrefix <> ensureStartsFromCapitalLetter qfml.family
        Nothing ->
            toolkitModulePrefix <> ensureStartsFromCapitalLetter qfml.family


familyModuleName :: ToolkitName -> QD.QFamily -> String
familyModuleName tkName qfml =
    case qfml.tag of
        Just familyTag ->
            toolkitModuleName tkName <> ".Family." <> ensureStartsFromCapitalLetter familyTag <> "." <> familyTypePrefix <> ensureStartsFromCapitalLetter qfml.family
        Nothing ->
            toolkitModuleName tkName <> ".Family." <> familyTypePrefix <> ensureStartsFromCapitalLetter qfml.family



familyTypeName :: QD.QFamily -> String
familyTypeName qfml =
    familyTypePrefix <> ensureStartsFromCapitalLetter qfml.family


familyModuleAlias :: QD.QFamily -> String
familyModuleAlias qfml =
    familyTypePrefix <> ensureStartsFromCapitalLetter qfml.family


familyNodeTypeName :: QD.QFamily -> String
familyNodeTypeName qfml =
    familyTypeName qfml <> "Node"


toolkitDataTypeName :: ToolkitName -> String
toolkitDataTypeName = unwrap >>> ensureStartsFromCapitalLetter


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


toolkitModulePrefix = "M" :: String
familyTypePrefix = "F" :: String
toolkitTypeNamePostfix = "Toolkit" :: String
toolkitModuleNamePostfix = "Gen" :: String
toolkitDataModuleNamePostfix = "GenData" :: String


i = "    " :: String
i2 = i <> i :: String
i3 = i2 <> i :: String
-- i4 = i3 <> i :: String
-- i5 = i4 <> i :: String
-- i6 = i5 <> i :: String


type Imports = Array String


familyModuleImports :: Imports
familyModuleImports =
    [ "Prelude (Unit, unit, ($), bind, pure)"
    , "Noodle.Fn2 as Fn"
    , "Noodle.Id (Input(..), Output(..)) as Fn"
    , "Noodle.Id (Family(..)) as Node"
    , "Noodle.Fn2.Process as P"
    , "Noodle.Family.Def as Family"
    , "Noodle.Node2 (Node) as N"
    ]


toolkitModuleImports :: Imports
toolkitModuleImports =
    [ "Prelude (Unit)"
    , "Noodle.Toolkit3 (Toolkit) as Noodle"
    , "Noodle.Toolkit3 as Toolkit"
    , "Noodle.Node2 (Node) as N"
    ]


allImports :: Imports -> String
allImports =
    map ((<>) "import ") >>> String.joinWith "\n"


familyModule :: ToolkitName -> LocalsPrefix -> Imports -> QD.QFamily -> String
familyModule tkName lp fmlUserImports qfml =
    "module " <> familyModuleName tkName qfml <> " (id, name, Family, family, Node, Inputs, Outputs, defaultInputs, defaultOutputs) where\n\n\n"
    <> allImports fmlUserImports <> "\n\n\n"
    <> allImports familyModuleImports <> "\n\n\n"
    <> "id = Node.Family :: _ \"" <> qfml.family <> "\"" <> "\n\n\n"
    <> "name :: String\n"
    <> "name = \"" <> qfml.family <> "\"" <> "\n\n\n"
    <> String.joinWith "\n" (inputProxyCode <$> qfml.inputs) <> "\n\n"
    <> String.joinWith "\n" (outputProxyCode <$> qfml.outputs) <> "\n\n\n"
    <> "type Inputs =\n"
    <> i2 <> (inBrackets (channelTypeAndLabel lp) ", " qfml.inputs) <> "\n\n"
    <> "type Outputs =\n"
    <> i2 <> (inBrackets (channelTypeAndLabel lp) ", " qfml.outputs) <> "\n\n\n"
    <> "defaultInputs :: Record Inputs\n"
    <> "defaultInputs =\n"
    <> i2 <> (inCBraces (channelDefaultAndLabel lp) ", " qfml.inputs) <> "\n\n"
    <> "defaultOutputs :: Record Outputs\n"
    <> "defaultOutputs =\n"
    <> i2 <> (inCBraces (channelDefaultAndLabel lp) ", " qfml.outputs) <> "\n\n\n"
    <> familyType lp qfml <> "\n\n\n"
    <> familyImplementation lp qfml <> "\n\n"
    <> nodeType lp qfml
    where
        inputProxyCode (Just ch) = "_in_" <> ch.name <> " = Fn.Input :: _ \"" <> ch.name <> "\""
        inputProxyCode Nothing = ""
        outputProxyCode (Just ch) = "_out_" <> ch.name <> " = Fn.Output :: _ \"" <> ch.name <> "\""
        outputProxyCode Nothing = ""


toolkitModule :: Way -> ToolkitName -> LocalsPrefix -> Imports -> Array QD.QFamily -> String
toolkitModule FamiliesAsModules tkName _ tkUserImports families =
    "module " <> toolkitModuleName tkName <> " (" <> toolkitTypeName tkName <> ", Toolkit, toolkit, Instances) where\n\n\n"
    <> allImports tkUserImports <> "\n\n\n"
    <> allImports toolkitModuleImports <> "\n\n\n"
    <> importAllFamilies tkName families <> "\n\n\n"
    <> toolkitType tkName families <> "\n\n\n"
    <> toolkitImplementation tkName families <> "\n\n\n"
    <> "type Toolkit m = " <> toolkitTypeName tkName <> " m" <> "\n\n\n"
    <> instancesType tkName families
toolkitModule FamiliesInline tkName lp tkUserImports families =
    "module " <> toolkitModuleName tkName <> " where\n\n"
    <> allImports tkUserImports <> "\n\n\n"
    <> allImports toolkitModuleImports <> "\n\n\n"
    <> familiesInlineImplementations lp families <> "\n\n\n"
    <> toolkitTypeInline tkName families <> "\n\n\n"
    <> toolkitImplementationInline tkName families <> "\n\n\n"
    <> "type Toolkit m = " <> toolkitTypeName tkName <> " m" <> "\n\n\n"
    <> nodesInlineTypes lp families <> "\n\n\n"
    <> instancesTypeInline tkName families <> "\n\n\n"


toolkitDataModule :: ToolkitName -> Array QD.QFamily -> String
toolkitDataModule tkName families =
    "module " <> toolkitDataModuleName tkName <> " where\n\n\n"
    <> toolkitSumType tkName families


toolkitSumType :: ToolkitName -> Array QD.QFamily -> String
toolkitSumType tkName families =
    "data " <> toolkitDataTypeName tkName <> "\n"
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
toolkitTypeInline tkName fmls =
    "type " <> toolkitTypeName tkName <> " m\n" <> i <> "= Noodle.Toolkit Unit\n"
        <> i2 <> inBrackets' familyInlineImplementationReferenceAndLabel ("\n" <> i2 <> ", ") fmls


toolkitType :: ToolkitName -> Array QD.QFamily -> String
toolkitType tkName fmls =
    "type " <> toolkitTypeName tkName <> " m\n" <> i <> "= Noodle.Toolkit Unit\n"
        <> i2 <> inBrackets' familyTypeReference ("\n" <> i2 <> ", ") fmls


toolkitImplementationInline :: ToolkitName -> Array QD.QFamily -> String
toolkitImplementationInline tkName fmls =
    "toolkit :: forall m. " <> toolkitTypeName tkName <> " m\n" <>
    "toolkit =\n"
        <> i <> "Toolkit.from \"" <> unwrap tkName <> "\"\n"
        <> i2 <> inCBraces' familyInlineImplementationReferenceAndLabel ("\n" <> i2 <> ", ") fmls


toolkitImplementation :: ToolkitName -> Array QD.QFamily -> String
toolkitImplementation tkName fmls =
    "toolkit :: forall m. " <> toolkitTypeName tkName <> " m\n" <>
    "toolkit =\n"
        <> i <> "Toolkit.from \"" <> unwrap tkName <> "\"\n"
        <> i2 <> inCBraces' familyModuleImplementationReferenceAndLabel ("\n" <> i2 <> ", ") fmls


importAllFamilies :: ToolkitName -> Array QD.QFamily -> String
importAllFamilies tkName fmls =
    String.joinWith "\n" (familyModuleImport tkName <$> fmls)


familiesInlineImplementations :: LocalsPrefix -> Array QD.QFamily -> String
familiesInlineImplementations lp fmls =
    String.joinWith "\n\n" (familyImplementationInline lp <$> fmls) <> "\n\n"


familyTypeConstructor :: QD.QFamily -> String
familyTypeConstructor qfml =
    -- unwrap >>> _.family >>> familyModuleName'
    ensureStartsFromCapitalLetter qfml.family


familyModuleImport :: ToolkitName -> QD.QFamily -> String
familyModuleImport tkName family =
   "import " <> familyModuleName tkName family <> " as " <> familyModuleAlias family


-- familyTypeAndImplementationInline :: QD.QFamily -> String
-- familyTypeAndImplementationInline qfml =
--     qfml.family <> " :: -- {-> " <> qfml.tag <> " <-}\n"
--         <> i3 <> "Family.Def Unit\n"
--         <> i4 <> (inBrackets channelTypeAndLabel ", " qfml.inputs) <> "\n"
--         <> i4 <> (inBrackets channelTypeAndLabel ", " qfml.outputs) <> "\n"
--         <> i4 <> "m"

familyType :: LocalsPrefix -> QD.QFamily -> String
familyType lp qfml =
    "type Family"
        <> " m =" <> tagComment qfml.tag <> "\n"
        <> i <> "Family.Def Unit\n"
        <> i2 <> "Inputs" <> "\n"
        <> i2 <> "Outputs" <> "\n"
        <> i2 <> "m"


familyTypeReference :: QD.QFamily -> String
familyTypeReference qfml =
    qfml.family <> " :: "
        <> familyModuleAlias qfml <> ".Family m"
        <> tagComment qfml.tag


familyInlineImplementationReferenceAndLabel :: QD.QFamily -> String
familyInlineImplementationReferenceAndLabel qfml =
    qfml.family <> " :" <> tagComment qfml.tag


familyModuleImplementationReferenceAndLabel :: QD.QFamily -> String
familyModuleImplementationReferenceAndLabel qfml =
    qfml.family <> " : (" <> familyModuleAlias qfml <> ".family :: " <> familyModuleAlias qfml <> ".Family m )"


instancesType :: ToolkitName -> Array (QD.QFamily) -> String
instancesType _ fmls =
    "type Instances m =\n"
        <> i2 <> inBrackets' instancesReferenceAndLabel ("\n" <> i2 <> ", ") fmls


instancesTypeInline :: ToolkitName -> Array (QD.QFamily) -> String
instancesTypeInline _ fmls =
    "type Instances m =\n"
        <> i2 <> inBrackets' instancesInlineReferenceAndLabel ("\n" <> i2 <> ", ") fmls


instancesReferenceAndLabel :: QD.QFamily -> String
instancesReferenceAndLabel qfml =
    qfml.family <> " :: Array ( " <> familyModuleAlias qfml <> ".Node m )"


instancesInlineReferenceAndLabel :: QD.QFamily -> String
instancesInlineReferenceAndLabel qfml =
    qfml.family <> " :: Array ( " <> familyNodeTypeName qfml <> " m )"


processBody :: String -> QD.QFamily -> String
processBody indent qfml =
    if (Array.length qfml.inputs > 0) || (Array.length qfml.outputs > 0) then
        ("do\n"
            <> (if Array.length qfml.inputs > 0 then
                    (indent <> String.joinWith ("\n" <> indent) (inputReceive <$> qfml.inputs) <> "\n")
                else "")
            <> indent <> outputComment <> "\n"
            <> (if Array.length qfml.outputs > 0 then
                    (indent <> String.joinWith ("\n" <> indent) (outputSend qfml.impl <$> qfml.outputs) <> "\n")
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
        outputSend Nothing Nothing = "--"
        outputSend (Just impl) Nothing = String.trim impl
        outputSend (Just impl) (Just ch) = "P.send _out_" <> ch.name <> " $ " <> String.trim impl
        outputSend Nothing (Just ch) = "P.send _out_" <> ch.name <> " ?out_" <> ch.name


familyImplementation :: LocalsPrefix -> QD.QFamily -> String
familyImplementation lp qfml =
    "family :: forall m. Family m\n"
    <> "family =" <> tagComment qfml.tag <> "\n"
    <> i <> "Family.def\n"
    <> i2 <> "unit\n"
    <> i2 <> "defaultInputs" <> "\n"
    <> i2 <> "defaultOutputs" <> "\n"
    <> i2 <> "$ Fn.make \"" <> qfml.family <> "\" $ " <> processBody i3 qfml


familyImplementationInline :: LocalsPrefix -> QD.QFamily -> String
familyImplementationInline lp qfml =
    "type " <> familyTypeName qfml <> " m =\n"
    <> i <> "Family.Def Unit\n"
    <> i2 <> (inBrackets (channelTypeAndLabel lp) ", " qfml.inputs) <> "\n"
    <> i2 <> (inBrackets (channelTypeAndLabel lp) ", " qfml.outputs) <> "\n"
    <> i2 <> "m" <> "\n\n"
    <> qfml.family <> " :: forall m. " <> familyTypeName qfml <> " m\n"
    <> qfml.family <> " =" <> tagComment qfml.tag <> "\n"
    <> i <> "Family.def\n"
    <> i2 <> "unit\n"
    <> i2 <> (inCBraces (channelDefaultAndLabel lp) ", " qfml.inputs) <> "\n"
    <> i2 <> (inCBraces (channelDefaultAndLabel lp) ", " qfml.outputs) <> "\n"
    <> i2 <> "$ Fn.make \"" <> qfml.family <> "\" $ " <> processBody i3 qfml


nodesInlineTypes :: LocalsPrefix -> Array (QD.QFamily) -> String
nodesInlineTypes lp fmls =
    String.joinWith "\n\n" (nodeTypeInline lp <$> fmls) <> "\n\n"


nodeType :: LocalsPrefix -> QD.QFamily -> String
nodeType lp qfml =
    "type Node m =\n"
    <> i <> "N.Node \"" <> qfml.family <> "\" Unit\n"
    <> i2 <> "Inputs" <> "\n"
    <> i2 <> "Outputs" <> "\n"
    <> i2 <> "m"


nodeTypeInline ::  LocalsPrefix -> QD.QFamily -> String
nodeTypeInline lp qfml =
    "type " <> familyNodeTypeName qfml <> " m =\n"
    <> i <> "N.Node \"" <> qfml.family <> "\" Unit\n"
    <> i2 <> (inBrackets (channelTypeAndLabel lp) ", " qfml.inputs) <> "\n"
    <> i2 <> (inBrackets (channelTypeAndLabel lp) ", " qfml.outputs) <> "\n"
    <> i2 <> "m"


channelTypeAndLabel :: LocalsPrefix -> Maybe QD.Channel -> String
channelTypeAndLabel lPrefix (Just ch) =
    ch.name <> " :: " <> fromMaybe "Unknown" ((<>) (unwrap lPrefix) <$> ch.type)
channelTypeAndLabel _ Nothing =
    "?ch_type"


channelDefaultAndLabel :: LocalsPrefix -> Maybe QD.Channel -> String
channelDefaultAndLabel lPrefix (Just ch) =
    ch.name <> " : " <> (fromMaybe ("?" <> ch.name <> "_default") ((<>) (unwrap lPrefix) <$> ch.default))
channelDefaultAndLabel _ Nothing =
    "?ch_default"


tagComment :: Maybe String -> String
tagComment Nothing = ""
tagComment (Just tag) = " -- {-> " <> tag <> " <-}"