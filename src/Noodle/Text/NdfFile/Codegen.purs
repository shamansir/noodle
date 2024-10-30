module Noodle.Text.NdfFile.Codegen where

import Prelude

import Partial.Unsafe (unsafePartial)

import Data.Map (Map)
import Data.Map (empty, insert, toUnfoldable, values) as Map
import Data.Maybe (Maybe(..))
import Data.Foldable (foldr)
import Data.Newtype (unwrap, wrap, class Newtype)
import Data.Tuple.Nested ((/\), type (/\))
import Data.List (toUnfoldable) as List
import Data.Array as Array

import Noodle.Toolkit (Name) as Toolkit
import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile (loadDefinitions) as NdfFile
import Noodle.Text.NdfFile.Types (NodeFamily, FamilyGroup)
import Noodle.Text.NdfFile.NodeDef (NodeDef(..))
import Noodle.Text.NdfFile.NodeDef (group, family) as NodeDef
import Noodle.Text.NdfFile.NodeDef.Codegen as CG

import PureScript.CST.Types (ImportDecl, Module(..))
import PureScript.CST.Types (Type, Expr, Declaration) as CST

import Tidy.Codegen -- hiding (importType, importTypeOp, importValue, importTypeAll)
-- import Tidy.Codegen.Monad (codegenModule, importFrom, importOpen, importType, importTypeAll, importTypeOp, importValue)



--newtype RootDir = FileName String
newtype FilePath = FilePath String
newtype FileContent = FileContent String
newtype GenRootPath = GenRootPath String
newtype ModulePrefix = ModulePrefix String
derive instance Newtype GenRootPath _
derive instance Newtype ModulePrefix _


derive newtype instance Eq FilePath
derive newtype instance Ord FilePath


codegen :: forall repr. CG.CodegenRepr repr => GenRootPath -> Toolkit.Name -> CG.Options repr -> NdfFile -> Map FilePath FileContent
codegen genRoot tkName options ndfFile =
    NdfFile.loadDefinitions ndfFile
    # foldr genModule Map.empty
    # Map.insert
        (FilePath $ toolkitFile genRoot tkName)
        (generateToolkit tkName options ndfFile)
    where
        filePathFor = FilePath <<< modulePath genRoot tkName
        genModule (NodeDef nodeDef) =
            Map.insert (filePathFor $ NodeDef nodeDef) $ FileContent $ CG.generate options nodeDef


generateToolkit :: forall repr. CG.CodegenRepr repr => Toolkit.Name -> CG.Options repr -> NdfFile -> FileContent
generateToolkit tkName options ndfFile = FileContent $ printModule $ generateToolkitModule tkName options ndfFile


generateToolkitModule :: forall repr. CG.CodegenRepr repr => Toolkit.Name -> CG.Options repr -> NdfFile -> Module Void
generateToolkitModule tkName (CG.Options opts) ndfFile
    = unsafePartial $ module_ (toolkitModuleName tkName)
        [ ]
        (
            [ declImport "Effect" [ importType "Effect" ]
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
        [ declTypeSignature (tkName <> "Families") $ typeCtor "Families"
        , declType (tkName <> "Families") [] familiesTList
        , declSignature "toolkit"
            $ typeApp (typeCtor "Toolkit")
                [ typeCtor $ tkName <> "Families"
                , typeCtor opts.reprAt.type_
                , typeCtor $ opts.monadAt.type_
                ]
        , declValue "toolkit" [] registerFamilies
        ]
    where
        groupAndFamily :: NodeFamily /\ NodeDef -> FamilyGroup /\ NodeFamily
        groupAndFamily (family /\ NodeDef ndef) = ndef.group /\ family
        definitions :: Array (FamilyGroup /\ NodeFamily)
        definitions = groupAndFamily <$> (Map.toUnfoldable $ NdfFile.loadDefinitions ndfFile)
        referFamily :: FamilyGroup /\ NodeFamily -> String
        referFamily (group /\ family) = unwrap group <> "." <> unwrap family <> "." <> "family"
        referFamilyF :: FamilyGroup /\ NodeFamily -> String
        referFamilyF (group /\ family) = unwrap group <> "." <> unwrap family <> "." <> "F"
        defToModuleImport :: Partial => FamilyGroup /\ NodeFamily -> ImportDecl Void
        defToModuleImport (group /\ family) =
            declImportAs
                (opts.nodeModuleName group family)
                []
                (unwrap group <> "." <> unwrap family)
        familiesTList :: Partial => CST.Type Void
        familiesTList =
            case Array.uncons definitions of
                Just { head, tail } ->
                    typeOp (typeCtor $ referFamilyF head)
                        $ (binaryOp ":>" <$> typeCtor <$> referFamilyF <$> tail)
                Nothing -> typeCtor "TNil"
        registerFamilies :: Partial => CST.Expr Void
        registerFamilies =
            exprOp
                (exprApp (exprIdent "Toolkit.empty") [ exprString tkName ])
                (binaryOp "#"
                    <$> exprApp (exprIdent "Toolkit.register")
                    <$> Array.singleton
                    <$> exprIdent
                    <$> referFamily
                    <$> definitions
                )



moduleName :: Toolkit.Name -> FamilyGroup -> NodeFamily -> String
moduleName tkName group family =
  tkName <> "." <> CG.groupPascalCase group <> "." <> CG.familyPascalCase family


moduleName' :: ModulePrefix -> Toolkit.Name -> FamilyGroup -> NodeFamily -> String
moduleName' modPrefix tkName group family =
  unwrap modPrefix <> "." <> moduleName tkName group family


modulePath :: GenRootPath -> Toolkit.Name -> NodeDef -> String
modulePath genRoot tkName nodeDef =
  unwrap genRoot <> "/" <> tkName <> "/" <> (CG.groupPascalCase $ NodeDef.group nodeDef)


moduleFile :: GenRootPath -> Toolkit.Name -> NodeDef -> String
moduleFile genRoot tkName nodeDef =
  modulePath genRoot tkName nodeDef <> "/" <> (CG.familyPascalCase $ NodeDef.family nodeDef) <> ".purs"


toolkitModuleName :: Toolkit.Name -> String
toolkitModuleName tkName =
    tkName <> "." <> "Toolkit"


toolkitModuleName' :: ModulePrefix -> Toolkit.Name -> String
toolkitModuleName' modPrefix tkName =
    unwrap modPrefix <> "." <> toolkitModuleName tkName


toolkitPath :: GenRootPath -> Toolkit.Name -> String
toolkitPath genRoot tkName = unwrap genRoot <> "/" <> tkName


toolkitFile :: GenRootPath -> Toolkit.Name -> String
toolkitFile genRoot tkName =
    toolkitPath genRoot tkName <> "/" <> tkName <> ".purs"
