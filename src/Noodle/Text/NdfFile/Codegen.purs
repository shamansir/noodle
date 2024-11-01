module Noodle.Text.NdfFile.Codegen where

import Prelude

import Partial.Unsafe (unsafePartial)

import Data.Map (Map)
import Data.Map (empty, insert) as Map
import Data.Maybe (Maybe(..))
import Data.Foldable (foldr)
import Data.Newtype (unwrap, wrap, class Newtype)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (uncons, reverse, singleton) as Array

import Noodle.Id (FamilyR, GroupR)
import Noodle.Toolkit (Name) as Toolkit
import Noodle.Id (toolkit) as Id
import Noodle.Text.NdfFile.FamilyDef (FamilyDef(..))
import Noodle.Text.NdfFile.FamilyDef (group, family) as FamilyDef
import Noodle.Text.NdfFile.FamilyDef.Codegen as CG

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


codegen :: forall repr. CG.CodegenRepr repr => Toolkit.Name -> CG.Options repr -> Array FamilyDef -> Map FilePath FileContent
codegen tkName options definitions =
    definitions
    # foldr genModule Map.empty
    # Map.insert
        (FilePath $ toolkitFile genRoot tkName)
        (generateToolkit tkName options definitions)
    where
        genRoot = GenRootPath ""
        filePathFor = FilePath <<< moduleFile genRoot tkName
        genModule (FamilyDef familyDef) =
            -- toCode (ToCode.pureScript) genOptions familyDef
            Map.insert (filePathFor $ FamilyDef familyDef) $ FileContent $ CG.generate options familyDef


generateToolkit :: forall repr. CG.CodegenRepr repr => Toolkit.Name -> CG.Options repr -> Array FamilyDef -> FileContent
generateToolkit tkName options = FileContent <<< printModule <<< generateToolkitModule tkName options


generateToolkitModule :: forall repr. CG.CodegenRepr repr => Toolkit.Name -> CG.Options repr -> Array FamilyDef -> Module Void
generateToolkitModule tkName (CG.Options opts) definitionsArray
    = unsafePartial $ module_ (toolkitModuleName tkName)
        [ ]
        (
            [ declImport "Prelude" [ importOp "#" ]
            , declImport "Effect" [ importType "Effect" ]
            , declImport "Type.Data.List" [ importTypeOp ":>" ]
            , declImport "Type.Data.List.Extra" [ importType "TNil", importClass "Put" ]
            , declImport "Noodle.Toolkit" [ importType "Toolkit" ]
            , declImportAs "Noodle.Toolkit" [ importValue "empty", importValue "register" ] "Toolkit"
            , declImport "Noodle.Toolkit.Families" [ importType "Families", importType "F", importClass "RegisteredFamily" ]
            ]
            <> (defToModuleImport <$> definitions) <>
            [ declImport opts.reprAt.module_ [ importType opts.reprAt.type_ ]
            ]
        )
        [ declTypeSignature (Id.toolkit tkName <> "Families") $ typeCtor "Families"
        , declType (Id.toolkit tkName <> "Families") [] familiesTList
        , declSignature "toolkit"
            $ typeApp (typeCtor "Toolkit")
                [ typeCtor $ Id.toolkit tkName <> "Families"
                , typeCtor opts.reprAt.type_
                , typeCtor $ opts.monadAt.type_
                ]
        , declValue "toolkit" [] registerFamilies
        ]
    where
        groupAndFamily :: FamilyDef -> GroupR /\ FamilyR
        groupAndFamily ndef = FamilyDef.group ndef /\ FamilyDef.family ndef
        definitions :: Array (GroupR /\ FamilyR)
        definitions = groupAndFamily <$> definitionsArray
        fModuleName :: GroupR /\ FamilyR -> String
        fModuleName (group /\ family) = CG.groupPascalCase group <> "." <> CG.familyPascalCase family
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
                (exprApp (exprIdent "Toolkit.empty") [ exprString $ Id.toolkit tkName ])
                (binaryOp "#"
                    <$> exprApp (exprIdent "Toolkit.register")
                    <$> Array.singleton
                    <$> exprIdent
                    <$> referFamily
                    <$> Array.reverse definitions
                )



moduleName :: Toolkit.Name -> GroupR -> FamilyR -> String
moduleName tkName group family =
  Id.toolkit tkName <> "." <> CG.groupPascalCase group <> "." <> CG.familyPascalCase family


moduleName' :: ModulePrefix -> Toolkit.Name -> GroupR -> FamilyR -> String
moduleName' modPrefix tkName group family =
  unwrap modPrefix <> "." <> moduleName tkName group family


modulePath :: GenRootPath -> Toolkit.Name -> FamilyDef -> String
modulePath genRoot tkName familyDef =
  unwrap genRoot <> "/" <> Id.toolkit tkName <> "/" <> (CG.groupPascalCase $ FamilyDef.group familyDef)


moduleFile :: GenRootPath -> Toolkit.Name -> FamilyDef -> String
moduleFile genRoot tkName familyDef =
  modulePath genRoot tkName familyDef <> "/" <> (CG.familyPascalCase $ FamilyDef.family familyDef) <> ".purs"


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
