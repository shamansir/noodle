module Noodle.Text.ParseSketch where

import Prelude

import Data.Either (Either(..))

import Effect (Effect)


import Effect.Console as Console

import Parsing (runParser)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Toolkit.Hydra2.Lang.ToCode (pureScript, toCode, javaScript)
import Noodle.Text.SketchParser as Parser


import Data.Traversable (traverse_)
import Data.Foldable (fold)
import Data.String as String

import Options.Applicative as OA
import Options.Applicative ((<**>))

import Node.FS.Stats (isDirectory)
-- import Node.FS.Aff (mkdir, rmdir, readTextFile, writeTextFile, appendTextFile, stat)


type Options =
    { sourceFile :: String
    , sourceList :: String
    , outputFormat :: String
    , targetPath :: String
    }


main :: Effect Unit
main = run =<< OA.execParser opts
  where -- FIXME: why it shows `run.js` in the info?
    opts = OA.info (options <**> OA.helper)
      ( OA.fullDesc
     <> OA.progDesc "Parse Hydra sketch and convert it to either PureScript code or NDF file (NYI), or back to JS code "
     <> OA.header "parse-sketch - Hydra sketches parser"
      )


run :: Options -> Effect Unit
run opts = do
    -- launchAff_ $ do
        -- liftEffect $ do
        traverse_ Console.log
            [ "parse-sketch - Hydra sketches parser"
            , "--help for help"
            , "\n----------------\n"
            , "Source file: " <> if not String.null opts.sourceList then "<list>" else opts.sourceFile
            , "Source list: " <> if String.null opts.sourceList then "<none>" else opts.sourceList
            , "Output format: " <> opts.outputFormat
            , "Target path: " <> opts.targetPath
            ]
        fileContents <- readTextFile UTF8 opts.sourceFile
        let parseResult = runParser (Parser.prepare fileContents) Parser.script
        case Parser.fixback <$> parseResult of
          Right script -> do
              Console.log "ORIGINAL: ======\n"
              Console.log fileContents
              Console.log "EXPR: ======\n"
              Console.log $ show script
              Console.log "PURS ======\n"
              Console.log $ toCode pureScript script
              Console.log "JS ======\n"
              Console.log $ toCode javaScript script
          Left error -> do
              Console.log "Parse failed."
              Console.log $ show error

    -- where
    --     toolkitName = QTG.ToolkitName opts.toolkitName
    --     localsPrefix = QTG.LocalsPrefix opts.localsPrefix
    --     rootDirPath = opts.rootDirectory
    --     genFamilyFile family =
    --         writeTextFile UTF8 (rootDirPath <> QTG.familyModulePath family) $ QTG.familyModule toolkitName localsPrefix opts.familyModuleImports family


options :: OA.Parser Options
options = ado
  sourceFile <- OA.strOption $ fold
    [ OA.long "source"
    , OA.short 'i'
    , OA.metavar "SOURCE"
    , OA.help "The name of the toolkit defines the name of the corresponding module and the directories to create and use"
    ]

  sourceList <- OA.strOption $ fold
    [ OA.long "source-list"
    , OA.short 'l'
    , OA.metavar "LIST_FILE"
    , OA.value ""
    , OA.help "The file where every line represents one source file to parse (empty by default)"
    , OA.showDefault
    ]

  outputFormat <- OA.strOption $ fold
    [ OA.long "output"
    , OA.short 'o'
    , OA.metavar "FORMAT"
    , OA.value "PURS"
    , OA.help "The format to output in : `PURS`, `JS` or `NDF`"
    ]

  targetPath <- OA.strOption $ fold
    [ OA.long "target-dir"
    , OA.short 'd'
    , OA.value ""
    , OA.help "Target directory where to store generated file(-s) (empty by default)"
    , OA.showDefault
    ]

  in
    { sourceFile
    , sourceList
    , outputFormat
    , targetPath
    }
