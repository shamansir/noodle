module Noodle.Text.GenerateToolkit where

import Prelude

import Data.Either (Either(..))
import Data.Traversable (traverse, traverse_)
import Data.Newtype (unwrap)
import Data.Array (fromFoldable)
import Data.Foldable (fold)

import Effect (Effect)
import Effect.Aff (launchAff, launchAff_, runAff_)
import Effect.Class (liftEffect)
import Effect.Console (log) as Console

import Text.Parsing.Parser as P
import Text.Parsing.Parser.String as P
import Text.Parsing.Parser.Token as P
import Text.Parsing.Parser.Combinators (choice, sepBy) as P

import Options.Applicative as OA
import Options.Applicative ((<**>))

import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir, readTextFile, writeTextFile, appendTextFile)

import Noodle.Text.Generators as QTG
import Noodle.Text.QuickDef as QD
import Noodle.Text.QuickDefParser as QDP


toolkitName = QTG.ToolkitName "hydra" :: QTG.ToolkitName
rootPath = "./src/Toolkit/HydraGen" :: String
toolkitDefPath = "./hydra.toolkit" :: String


type Options =
    { name :: String
    , root :: String
    , def :: String
    , keepRootDir :: Boolean
    , keepFamiliesDir :: Boolean
    , toolkitImports :: Array String
    , familyImports :: Array String
    }


main :: Effect Unit
main = run =<< OA.execParser opts
  where -- FIXME: why it shows `run.js` in the info?
    opts = OA.info (options <**> OA.helper)
      ( OA.fullDesc
     <> OA.progDesc "Generate PureScript sources for TOOLKIT node families using SOURCE text definition in special format (see ToolkitTextGen.md)"
     <> OA.header "generate-toolkit - toolkits code generator" )


run :: Options -> Effect Unit
run options =
    launchAff_ $ do
        liftEffect $ do
            traverse_ Console.log
                [ "generate-toolkit - toolkits code generator"
                , "--help for help"
                , "\n----------------\n"
                , "Definition file: " <> options.def
                , "Toolkit: " <> options.name
                , "Root: " <> options.root
                , "Keep root directory: " <> show options.keepRootDir
                , "Keep families directory: " <> show options.keepFamiliesDir
                , "Additional toolkit module imports: " <> show options.toolkitImports
                , "Additional family module imports: " <> show options.familyImports
                ]
        mkdir rootPath
        mkdir $ rootPath <> QTG.familiesModulesDirectoryPath
        quickDefsFile <- readTextFile UTF8 toolkitDefPath
        let parseResult = P.runParser quickDefsFile QDP.familyListParser
        case parseResult of
            Right familiesList -> do
                traverse_ genFamilyFile familiesList
                writeTextFile UTF8 (rootPath <> QTG.toolkitDataModulePath toolkitName) $ QTG.toolkitDataModule toolkitName familiesList
                writeTextFile UTF8 (rootPath <> QTG.toolkitModulePath toolkitName) $ QTG.toolkitModule QTG.FamiliesAsModules [] toolkitName familiesList
            Left error ->
                liftEffect $ Console.log $ show error
    where
        genFamilyFile family =
            writeTextFile UTF8 (rootPath <> QTG.familyModulePath family) $ QTG.familyModule toolkitName [] family


options :: OA.Parser Options
options = ado
  name <- OA.strOption $ fold
    [ OA.long "toolkit"
    , OA.short 't'
    , OA.metavar "TOOLKIT"
    , OA.help "The name of the toolkit defines the name of the corresponding module and the directories to create and use"
    ]

  root <- OA.strOption $ fold
    [ OA.long "dir"
    , OA.short 'd'
    , OA.metavar "DIR"
    , OA.value "."
    , OA.help "The directory where the toolkit `.purs` files will be generated"
    , OA.showDefault
    ]

  def <- OA.strOption $ fold
    [ OA.long "def"
    , OA.short 'x'
    , OA.metavar "SOURCE"
    , OA.help "The definition of the toolkit node families in the specific format, described in ToolkitTextGen.md"
    ]

  keepRootDir <- OA.switch $ fold
    [ OA.long "keep-root-dir"
    , OA.short 'k'
    , OA.help "Whether to clear and then create again the root directory (`keep` is off) when it doesn't exist, or not (`keep` is on), since clearing it may affect the files that are already there and used by toolkit"
    , OA.showDefault
    ]

  keepFamiliesDir <- OA.switch $ fold
    [ OA.long "keep-families-dir"
    , OA.short 'f'
    , OA.help "Whether to clear and then create again the families directory (`keep` is off) when it doesn't exist, or not (`keep` is on), since clearing it may affect the files that are already there and used by toolkit"
    , OA.showDefault
    ]

  {- enthusiasm <- option int $ fold
    [ long "enthusiasm"
    , help "How enthusiastically to greet"
    , showDefault
    , value 1
    , metavar "INT"
    ] -}

  toolkitImports <- map fromFoldable <$> OA.many $ OA.strOption $ fold
    [ OA.long "tk-import"
    , OA.short 'i'
    , OA.help "Module import to include in the generated toolkit file, i.e. `Data.Foldable` or `Data.Foldable (fold)` or `Data.Foldable as F`, or `Data.Foldable (fold) as F`"
    , OA.showDefault
    , OA.metavar "TOOLKIT_IMPORT"
    ]

  familyImports <- map fromFoldable <$> OA.many $ OA.strOption $ fold
    [ OA.long "fml-import"
    , OA.short 'm'
    , OA.help "Toolkit import to include in the generated family file, i.e. `Data.Foldable` or `Data.Foldable (fold)` or `Data.Foldable as F`, or `Data.Foldable (fold) as F`"
    , OA.showDefault
    , OA.metavar "FAMILY_IMPORT"
    ]

  in { name, root, def, keepRootDir, keepFamiliesDir, toolkitImports, familyImports }
