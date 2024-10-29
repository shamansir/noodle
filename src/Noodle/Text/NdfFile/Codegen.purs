module Noodle.Text.NdfFile.Codegen where

import Prelude

import Data.Map (Map)
import Data.Map (empty, insert) as Map
import Data.Foldable (foldr)
import Data.Newtype (unwrap, wrap, class Newtype)

import Noodle.Toolkit (Name) as Toolkit
import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile (loadDefinitions) as NdfFile
import Noodle.Text.NdfFile.Types (NodeFamily, FamilyGroup)
import Noodle.Text.NdfFile.NodeDef (NodeDef(..))
import Noodle.Text.NdfFile.NodeDef (group, family) as NodeDef
import Noodle.Text.NdfFile.NodeDef.Codegen as CG


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
    -- TODO: fill in toolkit module
    where
        filePathFor = FilePath <<< modulePath genRoot tkName
        genModule (NodeDef nodeDef) =
            Map.insert (filePathFor $ NodeDef nodeDef) $ FileContent $ CG.generate options nodeDef


moduleName :: Toolkit.Name -> FamilyGroup -> NodeFamily -> String
moduleName tkName group family =
  tkName <> "." <> CG.groupPascalCase group <> "." <> CG.familyPascalCase family


moduleName' :: ModulePrefix -> Toolkit.Name -> FamilyGroup -> NodeFamily -> String
moduleName' modPrefix tkName group family =
  unwrap modPrefix <> "." <> moduleName tkName group family


toolkitPath :: GenRootPath -> Toolkit.Name -> String
toolkitPath genRoot tkName = unwrap genRoot <> "/" <> tkName


toolkitFile :: GenRootPath -> Toolkit.Name -> String
toolkitFile genRoot tkName =
    toolkitPath genRoot tkName <> "/" <> tkName <> ".purs"


modulePath :: GenRootPath -> Toolkit.Name -> NodeDef -> String
modulePath genRoot tkName nodeDef =
  unwrap genRoot <> "/" <> tkName <> "/" <> (CG.groupPascalCase $ NodeDef.group nodeDef)


moduleFile :: GenRootPath -> Toolkit.Name -> NodeDef -> String
moduleFile genRoot tkName nodeDef =
  modulePath genRoot tkName nodeDef <> "/" <> (CG.familyPascalCase $ NodeDef.family nodeDef) <> ".purs"
