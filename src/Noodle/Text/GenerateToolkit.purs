module Noodle.Text.GenerateToolkit where

import Prelude

import Data.Either (Either(..))
import Data.Traversable (traverse, traverse_)
import Data.Newtype (unwrap)

import Effect (Effect)
import Effect.Aff (launchAff, launchAff_, runAff_)
import Effect.Class (liftEffect)
import Effect.Console (log) as Console

import Text.Parsing.Parser as P
import Text.Parsing.Parser.String as P
import Text.Parsing.Parser.Token as P
import Text.Parsing.Parser.Combinators (choice, sepBy) as P


import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir, readTextFile, writeTextFile, appendTextFile)


import Noodle.Text.Generators as QTG
import Noodle.Text.QuickDef as QD
import Noodle.Text.QuickDefParser as QDP


toolkitName = QTG.ToolkitName "hydra" :: QTG.ToolkitName
rootPath = "./src/Toolkit/HydraGen" :: String
toolkitDefPath = "./hydra.toolkit" :: String


main :: Effect Unit
main =
    launchAff_ $ do
        mkdir rootPath
        mkdir $ rootPath <> QTG.familiesModulesDirectoryPath
        quickDefsFile <- readTextFile UTF8 toolkitDefPath
        let parseResult = P.runParser quickDefsFile QDP.familyListParser
        case parseResult of
            Right familiesList -> do
                traverse_ genFamilyFile familiesList
                writeTextFile UTF8 (rootPath <> QTG.toolkitDataModulePath toolkitName) $ QTG.toolkitDataModule toolkitName familiesList
                writeTextFile UTF8 (rootPath <> QTG.toolkitModulePath toolkitName) $ QTG.toolkitModule QTG.FamiliesAsModules toolkitName familiesList
            Left error ->
                liftEffect $ Console.log $ show error
    where
        genFamilyFile family =
            writeTextFile UTF8 (rootPath <> QTG.familyModulePath family) $ QTG.familyModule toolkitName family
