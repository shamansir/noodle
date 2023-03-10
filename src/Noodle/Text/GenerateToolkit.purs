module Noodle.Text.GenerateToolkit where

import Prelude

import Data.Either (Either(..))
import Data.Traversable (traverse, traverse_)

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


toolkitName = "hydra"
toolkitNameCC = "Hydra"
toolkitPath = "./src/Toolkit/HydraGen"
toolkitDefPath = "./hydra.fn.clean.list"


main :: Effect Unit
main =
    launchAff_ $ do
        mkdir toolkitPath
        file <- readTextFile UTF8 toolkitDefPath
        let parseResult = P.runParser file QDP.familyListParser
        case parseResult of
            Right familiesList -> do
                traverse_ genFamilyFile familiesList
                pure unit
            Left error ->
                liftEffect $ Console.log $ show error
    where
        genFamilyFile family =
            writeTextFile UTF8 (toolkitPath <> "/" <> QTG.moduleName family <> ".purs") $ QTG.genFamilyModule "hydra" family
