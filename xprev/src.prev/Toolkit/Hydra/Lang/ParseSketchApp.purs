module Toolkit.Hydra.Lang.ParseSketchApp where

import Prelude

import Type.Proxy (Proxy)

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array as Array
import Data.Traversable (traverse_)
import Data.Foldable (fold)
import Data.String (null, toLower) as String
import Data.String.Utils (endsWith, lines, startsWith, charAt) as String
import Data.String.Extra (pascalCase) as String
import Data.String.CodeUnits (dropRight) as String

import Effect (Effect)
import Effect.Console as Console

import Parsing (runParser)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Path (FilePath, extname, basenameWithoutExt)

import Toolkit.Hydra.Lang.ToCode (class ToCode, pureScript, toCode, javaScript)
import Toolkit.Hydra.Lang.SketchParser as Parser

import Options.Applicative as OA
import Options.Applicative ((<**>))


-- import Node.FS.Aff (mkdir, rmdir, readTextFile, writeTextFile, appendTextFile, stat)


type Options =
    { sourceFile :: String
    , sourceList :: String
    , outputFormat :: String
    , targetPath :: String
    , customString :: String
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
    traverse_ Console.log
        [ "parse-sketch - Hydra sketches parser"
        , "--help for help"
        , "\n----------------\n"
        , "Source file: " <> if not $ String.null opts.sourceList then "<list>" else opts.sourceFile
        , "Source list: " <> if String.null opts.sourceList then "<none>" else opts.sourceList
        , "Output format: " <> if String.null opts.outputFormat then "<none>" else opts.outputFormat
        , "Target path: " <> if String.null opts.targetPath then "<none>" else opts.targetPath
        , "Custom string: " <> if String.null opts.customString then "<none>" else opts.customString
        ]
    if not $ String.null opts.sourceList then do
      listContents <- readTextFile UTF8 opts.sourceList
      let fileNames = Array.filter (not <<< String.startsWith "#") $ String.lines listContents
      traverse_ parseAndWrite fileNames
    else if not $ String.null opts.customString then
      logResult opts.customString (const "???") $ runParser (Parser.prepare opts.customString) $ Parser.sketch "CustomString"
    else do
      parseAndWrite opts.sourceFile
    where
      writeFile :: forall target. ToCode target Parser.Sketch => Proxy target -> Parser.Sketch -> String -> Effect Unit
      writeFile format sketch targetPath =
        writeTextFile UTF8 targetPath $ toCode format sketch
      parseAndWrite filePath = do
        fileContents <- readTextFile UTF8 filePath
        let
          targetPath format =
            let currentExt = extname filePath
            in case format of
                 "purs" -> if currentExt == ".js" then String.dropRight 3 filePath <> ".gen.purs" else filePath <> ".purs"
                 "js" -> if currentExt == ".js" then String.dropRight 3 filePath <> ".gen.js" else filePath
                 "ndf" -> if currentExt == ".js" then String.dropRight 3 filePath <> ".ndf" else filePath <> ".ndf"
                 "expr" -> if currentExt == ".js" then String.dropRight 3 filePath <> ".expr" else filePath <> ".expr"
                 _-> filePath
          parseResult =
            runParser (Parser.prepare fileContents) $ Parser.sketch $ moduleNameFromPath filePath
        logResult fileContents targetPath parseResult
      logResult fileContents targetPath parseResult =
        case Parser.fixback <$> parseResult of
          Right sketch ->
            case String.toLower opts.outputFormat of
              "purs" -> writeFile pureScript sketch $ targetPath "purs"
              "js" -> writeFile javaScript sketch $ targetPath "js"
              -- TODO: NDF
              _ ->
                do
                    Console.log $ targetPath "purs"
                    Console.log $ targetPath "js"
                    Console.log $ targetPath "ndf"
                    Console.log $ targetPath "expr"
                    Console.log "ORIGINAL: ======\n"
                    Console.log fileContents
                    Console.log "EXPR: ======\n"
                    Console.log $ show sketch
                    Console.log "PURS ======\n"
                    Console.log $ toCode pureScript sketch
                    Console.log "JS ======\n"
                    Console.log $ toCode javaScript sketch
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
    , OA.value ""
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
    , OA.value "CONSOLE"
    , OA.help "The format to output in : `PURS`, `JS` or `NDF` or `CONSOLE`"
    ]

  targetPath <- OA.strOption $ fold
    [ OA.long "target-dir"
    , OA.short 'd'
    , OA.value ""
    , OA.help "Target directory where to store generated file(-s) (empty by default)"
    , OA.showDefault
    ]

  customString <- OA.strOption $ fold
    [ OA.long "from"
    , OA.short 'f'
    , OA.value ""
    , OA.help "Custom string to parse instead of file"
    , OA.showDefault
    ]

  in
    { sourceFile
    , sourceList
    , outputFormat
    , targetPath
    , customString
    }


moduleNameFromPath :: FilePath -> String
moduleNameFromPath =
  ensureStartsWithLetter <<< String.pascalCase <<< flip basenameWithoutExt "js"
  where
    ensureStartsWithLetter str =
      case String.charAt 0 str of
        Just "0" -> addM str
        Just "1" -> addM str
        Just "2" -> addM str
        Just "3" -> addM str
        Just "5" -> addM str
        Just "6" -> addM str
        Just "7" -> addM str
        Just "8" -> addM str
        Just "9" -> addM str
        _ -> str
    addM = (<>) "M"