module Noodle.Text.GenerateToolkit where

import Prelude

import Data.Either (Either(..))
import Data.Traversable (traverse, traverse_)
import Data.Newtype (unwrap)
import Data.Array (fromFoldable)
import Data.Foldable (fold)
import Data.String as String

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
import Node.FS.Aff (mkdir, rmdir, readTextFile, writeTextFile, appendTextFile, exists)

import Noodle.Text.Generators as QTG
import Noodle.Text.QuickDef as QD
import Noodle.Text.QuickDefParser as QDP


type Options =
    { toolkitName :: String
    , rootDirectory :: String
    , definitionFile :: String
    , keepRootDirectory :: Boolean
    , keepFamiliesDirectory :: Boolean
    , toolkitModuleImports :: Array String
    , familyModuleImports :: Array String
    , localsPrefix :: String
    }


main :: Effect Unit
main = run =<< OA.execParser opts
  where -- FIXME: why it shows `run.js` in the info?
    opts = OA.info (options <**> OA.helper)
      ( OA.fullDesc
     <> OA.progDesc "Generate PureScript sources for TOOLKIT node families using SOURCE text definition in special format (see ToolkitTextGen.md)"
     <> OA.header "generate-toolkit - toolkits code generator"
      )


run :: Options -> Effect Unit
run opts =
    launchAff_ $ do
        liftEffect $ do
            traverse_ Console.log
                [ "generate-toolkit - toolkits code generator"
                , "--help for help"
                , "\n----------------\n"
                , "Definition file: " <> opts.definitionFile
                , "Toolkit: " <> opts.toolkitName
                , "Root: " <> opts.rootDirectory
                , "Keep root directory: " <> show opts.keepRootDirectory
                , "Keep families directory: " <> show opts.keepFamiliesDirectory
                , "Additional toolkit module imports: " <> show opts.toolkitModuleImports
                , "Additional family module imports: " <> show opts.familyModuleImports
                , "LocalsPrefix: " <> if String.null opts.localsPrefix then "none" else "\'" <> opts.localsPrefix <> "\'"
                ]
        when (not opts.keepRootDirectory) $ do
            rootDirExists <- exists rootDirPath
            when rootDirExists $ rmdir rootDirPath
            mkdir rootDirPath
        when (not opts.keepFamiliesDirectory) $ do
            let familyDirPath = rootDirPath <> QTG.familiesModulesDirectoryPath
            familyDirExists <- exists familyDirPath
            when familyDirExists $ rmdir familyDirPath
            mkdir familyDirPath
        quickDefsFile <- readTextFile UTF8 opts.definitionFile
        let parseResult = QDP.familyList quickDefsFile
        case parseResult of
            Right familiesList -> do
                traverse_ genFamilyFile familiesList
                writeTextFile UTF8 (rootDirPath <> QTG.toolkitDataModulePath toolkitName) $ QTG.toolkitDataModule toolkitName familiesList
                writeTextFile UTF8 (rootDirPath <> QTG.toolkitModulePath toolkitName) $ QTG.toolkitModule QTG.FamiliesAsModules toolkitName localsPrefix opts.toolkitModuleImports familiesList
            Left error ->
                liftEffect $ Console.log $ show error
    where
        toolkitName = QTG.ToolkitName opts.toolkitName
        localsPrefix = QTG.LocalsPrefix opts.localsPrefix
        rootDirPath = opts.rootDirectory
        genFamilyFile family =
            writeTextFile UTF8 (rootDirPath <> QTG.familyModulePath family) $ QTG.familyModule toolkitName localsPrefix opts.familyModuleImports family


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

  localsPrefix <- OA.strOption $ fold
    [ OA.long "locals-prefix"
    , OA.short 'l'
    , OA.metavar "LOCALS_PREFIX"
    , OA.value ""
    , OA.help "The prefix to add to all the local types and values, empty by default, could be, for example, `MT.` and this would help with family import `ToolkitTypesAndValues as MT`"
    , OA.showDefault
    ]

  in
    { toolkitName : name
    , rootDirectory : root
    , definitionFile : def
    , keepRootDirectory : keepRootDir
    , keepFamiliesDirectory : keepFamiliesDir
    , toolkitModuleImports : toolkitImports
    , familyModuleImports : familyImports
    , localsPrefix
    }
