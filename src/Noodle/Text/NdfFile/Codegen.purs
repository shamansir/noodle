module Noodle.Text.NdfFile.Codegen where

import Prelude

import Partial.Unsafe (unsafePartial)

import Data.Map (Map)
import Data.Map (empty, insert) as Map
import Data.Maybe (Maybe(..))
import Data.Foldable (foldr)
import Data.Newtype (unwrap, wrap, class Newtype)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (uncons, reverse, singleton) as Array
import Data.String (toUpper) as String

import Noodle.Id (FamilyR, GroupR)
import Noodle.Toolkit (Name) as Toolkit
import Noodle.Id (toolkit) as Id
import Noodle.Text.NdfFile.Types (Source)
import Noodle.Text.NdfFile.FamilyDef (FamilyDef(..))
import Noodle.Text.NdfFile.FamilyDef (group, family) as FamilyDef
import Noodle.Text.NdfFile.FamilyDef.Codegen as FCG

import PureScript.CST.Types (ImportDecl, Module)
import PureScript.CST.Types (Type, Expr) as CST

import Tidy.Codegen -- hiding (importType, importTypeOp, importValue, importTypeAll)
-- import Tidy.Codegen.Monad (codegenModule, importFrom, importOpen, importType, importTypeAll, importTypeOp, importValue)



newtype FilePath = FilePath String -- TODO: replace with Node.FS FilePath
newtype FileContent = FileContent String
newtype GenRootPath = GenRootPath String
newtype ModulePrefix = ModulePrefix String
derive instance Newtype GenRootPath _
derive instance Newtype ModulePrefix _


derive newtype instance Eq FilePath
derive newtype instance Ord FilePath


codegen
    :: forall repr
    .  FCG.CodegenRepr repr
    => Toolkit.Name
    -> FCG.Options repr
    -> Array (Maybe Source /\ FamilyDef)
    -> Map FilePath FileContent
codegen tkName options definitions =
    definitions
    # foldr genModule Map.empty
    # Map.insert
        (FilePath $ toolkitFile genRoot tkName)
        (generateToolkit tkName options $ Tuple.snd <$> definitions)
    where
        genRoot = GenRootPath ""
        filePathFor = FilePath <<< moduleFile genRoot tkName
        genModule (mbSource /\ FamilyDef familyDef) =
            -- toCode (ToCode.pureScript) genOptions familyDef
            Map.insert (filePathFor $ FamilyDef familyDef) $ FileContent $ FCG.generate options mbSource familyDef


generateToolkit :: forall repr. FCG.CodegenRepr repr => Toolkit.Name -> FCG.Options repr -> Array FamilyDef -> FileContent
generateToolkit tkName options = FileContent <<< printModule <<< generateToolkitModule tkName options


generateToolkitModule :: forall repr. FCG.CodegenRepr repr => Toolkit.Name -> FCG.Options repr -> Array FamilyDef -> Module Void
generateToolkitModule tkName (FCG.Options opts) definitionsArray
    = unsafePartial $ module_ (toolkitModuleName tkName)
        [ ]
        (
            [ declImport "Prelude" [ importOp "#" ]
            , declImport "Effect" [ importType "Effect" ]
            , declImportAs "Color" [] "Color"
            , declImport "Type.Data.List" [ importTypeOp ":>" ]
            , declImport "Type.Data.List.Extra" [ importType "TNil", importClass "Put" ]
            , declImport "Type.Proxy" [ importTypeAll "Proxy" ]
            , declImportAs "Noodle.Id" [ importValue "toolkitR" ] "Id"
            , declImport "Noodle.Toolkit" [ importType "Toolkit", importType "ToolkitKey", importClass "MarkToolkit" ]
            , declImportAs "Noodle.Toolkit" [ importValue "empty", importValue "register" ] "Toolkit"

            , declImport "Noodle.Toolkit.Families" [ importType "Families", importType "F", importClass "RegisteredFamily" ]
            ]
            <> (defToModuleImport <$> definitions) <>
            [ declImport opts.reprAt.module_ [ importType opts.reprAt.type_ ]
            ]
        )
        [ declTypeSignature familiesCtor $ typeCtor "Families"
        , declType familiesCtor [] familiesTList
        , declForeignData toolkitKey $ typeCtor "ToolkitKey"
        , declSignature "toolkit"
            $ typeApp (typeCtor "Toolkit")
                [ typeCtor toolkitKey
                , typeCtor familiesCtor
                , typeCtor opts.reprAt.type_
                , typeCtor $ opts.monadAt.type_
                ]
        , declValue "toolkit" [] registerFamilies
        , declInstance Nothing [] "MarkToolkit" [ typeCtor toolkitKey ]
            [ instValue "markGroup" [ binderWildcard, binderVar "group" ]
                $ exprApp (exprIdent "Color.rgb")
                    [ exprInt 255
                    , exprInt 255
                    , exprInt 255
                    ]
            , instValue "markFamily" [ binderWildcard, binderVar "family" ]
                $ exprApp (exprIdent "Color.rgb")
                    [ exprInt 255
                    , exprInt 255
                    , exprInt 255
                    ]
            ]
        ]
    where
        toolkitKey = String.toUpper $ Id.toolkit tkName -- Id.toolkit tkName <> "Key"
        familiesCtor = Id.toolkit tkName <> "Families"
        groupAndFamily :: FamilyDef -> GroupR /\ FamilyR
        groupAndFamily fdef = FamilyDef.group fdef /\ FamilyDef.family fdef
        definitions :: Array (GroupR /\ FamilyR)
        definitions = groupAndFamily <$> definitionsArray
        fModuleName :: GroupR /\ FamilyR -> String
        fModuleName (group /\ family) = FCG.groupPascalCase group <> "." <> FCG.familyPascalCase family
        referFamily :: GroupR /\ FamilyR -> String
        referFamily (group /\ family) = fModuleName (group /\ family) <> "." <> "family"
        referFamilyF :: GroupR /\ FamilyR -> String
        referFamilyF (group /\ family) = fModuleName (group /\ family) <> "." <> "F"
        defToModuleImport :: Partial => GroupR /\ FamilyR -> ImportDecl Void
        defToModuleImport (group /\ family) =
            declImportAs
                (opts.familyModuleName group family)
                []
                $ fModuleName (group /\ family)
        familiesTList :: Partial => CST.Type Void
        familiesTList =
            case Array.uncons definitions of
                Just { head, tail } ->
                    typeOp (typeCtor $ referFamilyF head)
                        $ (binaryOp ":>" <$> typeCtor <$> referFamilyF <$> tail)
                            <> [ binaryOp ":>" $ typeCtor "TNil"]
                Nothing -> typeCtor "TNil"
        registerFamilies :: Partial => CST.Expr Void
        registerFamilies =
            exprOp
                (exprApp (exprIdent "Toolkit.empty")
                    [ exprTyped
                        ( exprCtor "Proxy" )
                        $ typeApp typeWildcard [ typeCtor toolkitKey ]
                    , exprApp (exprIdent "Id.toolkitR")
                        [ exprString $ Id.toolkit tkName ]
                    ]
                )
                (binaryOp "#"
                    <$> exprApp (exprIdent "Toolkit.register")
                    <$> Array.singleton
                    <$> exprIdent
                    <$> referFamily
                    <$> Array.reverse definitions
                )



moduleName :: Toolkit.Name -> GroupR -> FamilyR -> String
moduleName tkName group family =
  Id.toolkit tkName <> "." <> FCG.groupPascalCase group <> "." <> FCG.familyPascalCase family


moduleName' :: ModulePrefix -> Toolkit.Name -> GroupR -> FamilyR -> String
moduleName' modPrefix tkName group family =
  unwrap modPrefix <> "." <> moduleName tkName group family


modulePath :: GenRootPath -> Toolkit.Name -> FamilyDef -> String
modulePath genRoot tkName familyDef =
  unwrap genRoot <> "/" <> Id.toolkit tkName <> "/" <> (FCG.groupPascalCase $ FamilyDef.group familyDef)


moduleFile :: GenRootPath -> Toolkit.Name -> FamilyDef -> String
moduleFile genRoot tkName familyDef =
  modulePath genRoot tkName familyDef <> "/" <> (FCG.familyPascalCase $ FamilyDef.family familyDef) <> ".purs"


toolkitModuleName :: Toolkit.Name -> String
toolkitModuleName tkName =
    Id.toolkit tkName <> "." <> "Toolkit"


toolkitModuleName' :: ModulePrefix -> Toolkit.Name -> String
toolkitModuleName' modPrefix tkName =
    unwrap modPrefix <> "." <> toolkitModuleName tkName


toolkitPath :: GenRootPath -> Toolkit.Name -> String
toolkitPath genRoot tkName = unwrap genRoot <> "/" <> Id.toolkit tkName


toolkitFile :: GenRootPath -> Toolkit.Name -> String
toolkitFile genRoot tkName =
    toolkitPath genRoot tkName <> "/" <> "Toolkit" <> ".purs"