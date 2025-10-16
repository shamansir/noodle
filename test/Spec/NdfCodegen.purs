module Test.Spec.NdfCodegen where

import Prelude

import Data.String as String
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Traversable (traverse, traverse_)
import Data.Array (length, take, dropEnd, fromFoldable) as Array
import Data.Map (Map)
import Data.Set (Set)
import Data.Map (keys, fromFoldable, toUnfoldable, empty, filter) as Map
import Data.Map.Extra (joinWith, withKeys') as MapX
import Data.Set (toUnfoldable) as Set
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (unwrap)

import Effect.Console as Console

import Control.Monad.Error.Class (class MonadThrow)

import Partial.Unsafe (unsafePartial)

import Type.Proxy (Proxy(..))

import Effect.Class (class MonadEffect, liftEffect)

import Parsing (runParser) as P

import Test.Spec (Spec, describe, it, itOnly)
import Test.Spec.Assertions (fail)

import Node.Encoding (Encoding(..))
import Node.Path (FilePath) as FS
import Node.FS.Sync (readTextFile, writeTextFile, exists, mkdir')
import Node.FS.Perms (permsReadWrite)

import Tidy.Codegen

import Noodle.Id (toolkitR, family) as Id
import Noodle.Fn.Shape.Temperament (defaultAlgorithm) as Temperament
import Noodle.Text.ToCode (toCode)
import Noodle.Text.Code.Target (pureScript) as ToCode
import Noodle.Text.NdfFile (loadOrder, hasFailedLines, failedLines, codegen) as NdfFile
import Noodle.Text.NdfFile.Codegen as MCG
import Noodle.Text.NdfFile.Parser (parser) as NdfFile
import Noodle.Text.NdfFile.FamilyDef (FamilyDef, chtv, i, o, qdefps, st) as FD
import Noodle.Text.NdfFile.FamilyDef.ProcessCode (ProcessCode(..)) as FD
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr, Options(..), withOptions) as FCG
-- import Noodle.Text.NdfFile.Codegen as MCG
import Noodle.Text.NdfFile.Codegen (toolkitModuleName) as CG
import Noodle.Toolkit (Name) as Toolkit

import Example.Toolkit.Minimal.ChRepr (MinimalVRepr)
import Example.Toolkit.Minimal.StRepr (MinimalStRepr)

import HydraTk.Types (FnArg(..))
import HydraTk.Repr.Wrap (WrapRepr)
import HydraTk.Repr.GenOptions (genOptions, GenOptions) as Hydra
import HydraTk.FnList as HydraFnList

import Test.Spec.Assertions (shouldEqual) as C
import Test.Spec.Util.Assertions (shouldEqual, shouldEqualStack) as U


minimalGenOptions :: FCG.Options MinimalStRepr MinimalVRepr
minimalGenOptions = FCG.Options
  { temperamentAlgorithm : Temperament.defaultAlgorithm
  , monadAt : { module_ : "Effect", type_ : "Effect" }
  , chreprAt : { module_ : "Example.Toolkit.Minimal.ChRepr", type_ : "MinimalVRepr" }
  , streprAt : { module_ : "Example.Toolkit.Minimal.ChRepr", type_ : "MinimalStRepr" }
  , pstreprType : "MinimalStRepr"
  , familyModuleName : \groupR familyR -> "OUTPUT." <> MCG.moduleName' modulePrefix (Id.toolkitR "Test") groupR familyR
  , toolkitModuleName : CG.toolkitModuleName
  , pstrepr : (Proxy :: _ MinimalStRepr)
  , pchrepr : (Proxy :: _ MinimalVRepr)
  , infoComment : Nothing
  , tkImports : unsafePartial $
    [ declImportAs "Data.String" [ importValue "length" ] "String"
    , declImport "Example.Toolkit.Minimal.ChRepr" [ importTypeAll "MinimalVRepr", importTypeAll "MinimalStRepr" ]
    ]
  , familyImports : Id.family >>> (unsafePartial $ case _ of
    "concat" ->
      [ declImportAs "Data.String" [ importValue "length" ] "String"
      ]
    _ -> [])
  }


spec :: Spec Unit
spec = do

    describe "NDF Codegen" $ do

      it "auto code definitions:" $ do
        -- FIXME: split in different tests
        -- (toCode (ToCode.pureScript) unit $ ND.Auto "") `U.shouldEqual` ""
        -- (toCode (ToCode.pureScript) unit $ ND.Auto "a") `U.shouldEqual` "a"
        (toCode (ToCode.pureScript) unit $ FD.Auto "outlet::<inlet1> + <inlet2>") `U.shouldEqual` """do
  inlet1 <- Fn.receive _in_inlet1
  inlet2 <- Fn.receive _in_inlet2
  Fn.send _out_outlet $ inlet1 + inlet2"""
        (toCode (ToCode.pureScript) unit $ FD.Auto "out::H.Start $ H.Solid { <r>, <g>, <b>, <a> }") `U.shouldEqual` """do
  r <- Fn.receive _in_r
  g <- Fn.receive _in_g
  b <- Fn.receive _in_b
  a <- Fn.receive _in_a
  Fn.send _out_out $ H.Start $ H.Solid { r, g, b, a }"""
        (toCode (ToCode.pureScript) unit $ FD.Auto "out::H.Start $ H.Solid { <r>, <g>, <b>, <a> };r::<r>;g::<g>;b::<b>;a::<a>") `U.shouldEqual` """do
  r <- Fn.receive _in_r
  g <- Fn.receive _in_g
  b <- Fn.receive _in_b
  a <- Fn.receive _in_a
  Fn.send _out_out $ H.Start $ H.Solid { r, g, b, a }
  Fn.send _out_r $ r
  Fn.send _out_g $ g
  Fn.send _out_b $ b
  Fn.send _out_a $ a"""
        (toCode (ToCode.pureScript) unit $ FD.Raw """do
    r <- Noodle.receive _in_r
    g <- Noodle.receive _in_g
    b <- Noodle.receive _in_b
    Noodle.send _out_color $
        VR.Color
            { r : floor $ r * 255.0
            , g : floor $ g * 255.0
            , b : floor $ b * 255.0
            , a: 255
            }""") `U.shouldEqual` """do
    r <- Noodle.receive _in_r
    g <- Noodle.receive _in_g
    b <- Noodle.receive _in_b
    Noodle.send _out_color $
        VR.Color
            { r : floor $ r * 255.0
            , g : floor $ g * 255.0
            , b : floor $ b * 255.0
            , a: 255
            }"""
        (toCode (ToCode.pureScript) unit $ FD.JS """
  const aValue = _receive("a");
  const bValue = _receive("b");
  _send("sum",
      { tag : "Int",
        value : (parseInt(aValue.value) + parseInt(bValue.value)).toString()
      });""") `U.shouldEqual` ("fromJsCode $ jsCode $\n\t\t\"\"\"" <> """
  const aValue = _receive("a");
  const bValue = _receive("b");
  _send("sum",
      { tag : "Int",
        value : (parseInt(aValue.value) + parseInt(bValue.value)).toString()
      });""" <> "\n\t\t\"\"\"")

      it "should compile to the expected code" $ do
        let
          testFamilyDef = FD.qdefps
            { group : "all", family : "concat"
            , inputs :
              [ FD.i $ FD.chtv "left" "String" ""
              , FD.i $ FD.chtv "right" "String" ""
              ]
            , outputs :
              [ FD.o $ FD.chtv "out" "String" ""
              , FD.o $ FD.chtv "len" "Int" "0"
              ]
            , state : FD.st "Unit" "unit"
            , process : FD.Raw """do
  left <- Fn.receive _in_left
  right <- Fn.receive _in_right
  let concatenated = left <> right
  Fn.send _out_out concatenated
  Fn.send _out_len $ String.length concatenated"""
            }

        testSingleFamilyDef (Id.toolkitR "Test") minimalGenOptions testFamilyDef

      {-
      it "writes NDF documentation" $ do
        let fnDocs = HydraFnList.toDocumentation HydraFnList.fns
        liftEffect $ writeTextFile UTF8 "./ndf/hydra-autodocs.ndf" fnDocs
      -}

      itOnly "properly generates Hydra Toolkit" $ do
        hydraToolkitText <- liftEffect $ readTextFile UTF8 "./ndf/hydra.v0.4.ndf"
        let eParsedNdf = P.runParser hydraToolkitText NdfFile.parser
        case eParsedNdf of
          Left error -> fail $ show error
          Right parsedNdf ->
            if not $ NdfFile.hasFailedLines parsedNdf then do
              let outputFilesMap = NdfFile.codegen (Id.toolkitR "Hydra") customHydraGenOptions parsedNdf
              traverse_ _writeOutputFile (Map.toUnfoldable outputFilesMap :: Array (MCG.FilePath /\ MCG.FileContent))
              inputFilesMap <- MapX.withKeys' _readInputFile outputFilesMap
              let
                results = MapX.joinWith (\output input -> { input, output }) outputFilesMap inputFilesMap
              --   allOutputFiles = Map.keys outputFilesMap
              --   onlyEqualFiles = Map.keys $ Map.filter __compareByEq results
              -- ( (String.joinWith "\n" $ show <$> Array.fromFoldable allOutputFiles)
              --    `U.shouldEqualStack`
              --   (String.joinWith "\n" $ show <$> Array.fromFoldable onlyEqualFiles)
              -- )
              traverse_ (uncurry testCodegenPair) $ (Map.toUnfoldable results :: Array (MCG.FilePath /\ { input :: MCG.FileContent, output :: MCG.FileContent }))
            else
              fail $ "Failed to parse starting at:\n" <> (String.joinWith "\n" $ show <$> (Array.take 3 $ NdfFile.failedLines parsedNdf))


__compareByEq :: { input :: MCG.FileContent, output :: MCG.FileContent } -> Boolean
__compareByEq content =
  case content.input /\ content.output of
    MCG.FileContent inputContent /\ MCG.FileContent outputContent ->
      outputContent == __alterInput inputContent


__alterInput :: String -> String
__alterInput =
  String.replaceAll (String.Pattern "INPUT.") (String.Replacement "OUTPUT.")


_readInputFile :: forall m. MonadEffect m => MCG.FilePath -> m MCG.FileContent
_readInputFile (MCG.FilePath filePath) =
  MCG.FileContent <$> (liftEffect $ readTextFile UTF8 $ unwrap inputDir <> filePath)


_writeOutputFile :: forall m. MonadEffect m => MCG.FilePath /\ MCG.FileContent -> m Unit
_writeOutputFile (MCG.FilePath filePath /\ MCG.FileContent fileContent) =
  let
    outputFilePath = unwrap outputDir <> filePath
    -- inputFilePath  = unwrap inputDir <> filePath
    outputDirectory = String.joinWith "/" $ Array.dropEnd 1 $ String.split (String.Pattern "/") outputFilePath
  in
  liftEffect $ do
    outputDirectoryExists <- exists outputDirectory
    when (not outputDirectoryExists) $ mkdir' outputDirectory { mode : permsReadWrite, recursive : true }
    writeTextFile UTF8 outputFilePath fileContent
    -- writeTextFile UTF8 inputFilePath fileContent


customHydraGenOptions :: Hydra.GenOptions
customHydraGenOptions =
  FCG.withOptions Hydra.genOptions $ \opts -> opts
      { familyModuleName = \groupR familyR -> "OUTPUT." <> MCG.moduleName' modulePrefix (Id.toolkitR "Hydra") groupR familyR
      }


modulePrefix = MCG.ModulePrefix "Test.Files.CodeGenTest" :: MCG.ModulePrefix


inputDir  = MCG.GenRootPath "./test/Files/Input"  :: MCG.GenRootPath
outputDir = MCG.GenRootPath "./test/Files/Output" :: MCG.GenRootPath


testSingleFamilyDef :: forall m strepr chrepr. Bind m => MonadEffect m => MonadThrow _ m => FCG.CodegenRepr strepr => FCG.CodegenRepr chrepr => Toolkit.Name -> FCG.Options strepr chrepr -> FD.FamilyDef -> m Unit
testSingleFamilyDef tkName genOptions familyDef =
  let
    filePath = MCG.moduleFile (MCG.GenRootPath "") tkName familyDef
    fileContent = toCode (ToCode.pureScript) genOptions familyDef
  in testCodegenSingleFile
     $ MCG.FilePath filePath /\ MCG.FileContent fileContent


testCodegenPair :: forall m. MonadEffect m => MCG.FilePath -> { input :: MCG.FileContent, output :: MCG.FileContent } -> m Unit
testCodegenPair (MCG.FilePath filePath) content = do
  let
    outputFilePath = unwrap outputDir <> filePath
    inputFilePath  = unwrap inputDir <> filePath
  case content.input /\ content.output of
    MCG.FileContent inputContent /\ MCG.FileContent outputContent ->
      liftEffect $ do
        Console.log $ inputFilePath <> " <-> " <> outputFilePath
        outputContent `U.shouldEqual` __alterInput inputContent


testCodegenSingleFile :: forall m. MonadEffect m => MCG.FilePath /\ MCG.FileContent -> m Unit
testCodegenSingleFile (MCG.FilePath filePath /\ MCG.FileContent fileContent) = do
  let
    outputFilePath = unwrap outputDir <> filePath
    inputFilePath  = unwrap inputDir <> filePath
    outputDirectory = String.joinWith "/" $ Array.dropEnd 1 $ String.split (String.Pattern "/") outputFilePath
  liftEffect $ do
    outputDirectoryExists <- exists outputDirectory
    when (not outputDirectoryExists) $ mkdir' outputDirectory { mode : permsReadWrite, recursive : true }
    writeTextFile UTF8 outputFilePath fileContent
    sample <- readTextFile UTF8 inputFilePath
    Console.log $ inputFilePath <> " <-> " <> outputFilePath
    fileContent `U.shouldEqual` __alterInput sample
