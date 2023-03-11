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


toolkitName = QTG.ToolkitName "hydra"
toolkitNameCC = QTG.ToolkitName "Hydra"
toolkitPath = "./src/Toolkit/HydraGen"
toolkitDefPath = "./hydra.fn.clean.list"


main :: Effect Unit
main =
    launchAff_ $ do
        mkdir toolkitPath
        mkdir $ toolkitPath <> "/Family"
        quickDefsFile <- readTextFile UTF8 toolkitDefPath
        let parseResult = P.runParser quickDefsFile QDP.familyListParser
        case parseResult of
            Right familiesList -> do
                traverse_ genFamilyFile familiesList
                writeTextFile UTF8 (toolkitPath <> "/" <> unwrap toolkitNameCC <> "Data.purs") $ QTG.toolkitDataModule toolkitName familiesList
                writeTextFile UTF8 (toolkitPath <> "/" <> unwrap toolkitNameCC <> ".purs") $ QTG.toolkitModule toolkitName familiesList
            Left error ->
                liftEffect $ Console.log $ show error
    where
        genFamilyFile family =
            writeTextFile UTF8 (toolkitPath <> "/Family/" <> QTG.familyModuleName family <> ".purs") $ QTG.familyModule toolkitName family
