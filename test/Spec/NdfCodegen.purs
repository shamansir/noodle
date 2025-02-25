module Test.Spec.NdfCodegen where

import Prelude

import Data.String as String
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Traversable (traverse_)
import Data.Array (length, take, dropEnd) as Array
import Data.Map (toUnfoldable) as Map
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (unwrap)

import Effect.Console as Console

import Control.Monad.Error.Class (class MonadThrow)

import Partial.Unsafe (unsafePartial)

import Type.Proxy (Proxy(..))

import Effect.Class (class MonadEffect, liftEffect)

import Parsing (runParser) as P

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

import Node.Encoding (Encoding(..))
import Node.Path (FilePath) as FS
import Node.FS.Sync (readTextFile, writeTextFile, exists, mkdir')
import Node.FS.Perms (permsReadWrite)

import Tidy.Codegen

import Noodle.Id (toolkitR) as Id
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
import Noodle.Toolkit (Name) as Toolkit

import Example.Toolkit.Minimal.Repr (MinimalStRepr, MinimalVRepr)

import Hydra.Types (FnArg(..))
import Hydra.Repr.Wrap (WrapRepr, hydraGenOptions)

import Test.Spec.Util.Assertions (shouldEqual) as U


minimalGenOptions :: FCG.Options MinimalStRepr MinimalVRepr
minimalGenOptions = FCG.Options
  { temperamentAlgorithm : Temperament.defaultAlgorithm
  , monadAt : { module_ : "Effect", type_ : "Effect" }
  , chreprAt : { module_ : "Example.Toolkit.Minimal.Repr", type_ : "MinimalVRepr" }
  , streprAt : { module_ : "Example.Toolkit.Minimal.Repr", type_ : "MinimalStRepr" }
  , familyModuleName : MCG.moduleName' modulePrefix $ Id.toolkitR "Test"
  , pstrepr : (Proxy :: _ MinimalStRepr)
  , pchrepr : (Proxy :: _ MinimalVRepr)
  , infoComment : Nothing
  , tkImports : unsafePartial $
    [ declImportAs "Data.String" [ importValue "length" ] "String"
    , declImport "Example.Toolkit.Minimal.Repr" [ importTypeAll "MinimalVRepr", importTypeAll "MinimalStRepr" ]
    ]
  , familyImports : const []
  }


spec :: Spec Unit
spec = do

    describe "NDF Codegen" $ do

      it "auto code definitions:" $ do
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

      it "properly generates Hydra Toolkit" $ do
        hydraToolkitText <- liftEffect $ readTextFile UTF8 "./ndf/hydra.v0.3.ndf"
        let eParsedNdf = P.runParser hydraToolkitText NdfFile.parser
        case eParsedNdf of
          Left error -> fail $ show error
          Right parsedNdf ->
            if not $ NdfFile.hasFailedLines parsedNdf then do
              --liftEffect $ Console.log $ show $ NdfFile.loadOrder parsedNdf
              let fileMap = NdfFile.codegen (Id.toolkitR "Hydra") customHydraGenOptions parsedNdf
              traverse_ testCodegenFile $ (Map.toUnfoldable fileMap :: Array (MCG.FilePath /\ MCG.FileContent))
            else
              fail $ "Failed to parse starting at:\n" <> (String.joinWith "\n" $ show <$> (Array.take 3 $ NdfFile.failedLines parsedNdf))


customHydraGenOptions :: FCG.Options WrapRepr WrapRepr
customHydraGenOptions =
  FCG.withOptions hydraGenOptions $ \opts -> opts
      { familyModuleName = MCG.moduleName' modulePrefix $ Id.toolkitR "Hydra"
      }


modulePrefix = MCG.ModulePrefix "Test.Files.CodeGenTest" :: MCG.ModulePrefix


inputDir  = MCG.GenRootPath "./test/Files/Input"  :: MCG.GenRootPath
outputDir = MCG.GenRootPath "./test/Files/Output" :: MCG.GenRootPath


testSingleFamilyDef :: forall m strepr chrepr. Bind m => MonadEffect m => MonadThrow _ m => FCG.CodegenRepr strepr => FCG.CodegenRepr chrepr => Toolkit.Name -> FCG.Options strepr chrepr -> FD.FamilyDef -> m Unit
testSingleFamilyDef tkName genOptions familyDef =
  let
    filePath = MCG.moduleFile (MCG.GenRootPath "") tkName familyDef
    fileContent = toCode (ToCode.pureScript) genOptions familyDef
  in testCodegenFile
     $ MCG.FilePath filePath /\ MCG.FileContent fileContent


testCodegenFile :: forall m. MonadEffect m => MCG.FilePath /\ MCG.FileContent -> m Unit
testCodegenFile (MCG.FilePath filePath /\ MCG.FileContent fileContent) = do
  let
    outputFilePath = unwrap outputDir <> filePath
    inputFilePath  = unwrap inputDir <> filePath
    outputDirectory = String.joinWith "/" $ Array.dropEnd 1 $ String.split (String.Pattern "/") outputFilePath
  liftEffect $ do
    outputDirectoryExists <- exists outputDirectory
    when (not outputDirectoryExists) $ mkdir' outputDirectory { mode : permsReadWrite, recursive : true }
    writeTextFile UTF8 outputFilePath fileContent
    sample <- readTextFile UTF8 inputFilePath
    let alteredSample =
          sample
          # String.replace (String.Pattern "CodeGenTest.Input") (String.Replacement "CodeGenTest")
          # String.replace (String.Pattern "Input.Hydra") (String.Replacement "Hydra")
    fileContent `U.shouldEqual` alteredSample
