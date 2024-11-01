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
import Node.FS.Sync (readTextFile, writeTextFile, mkdir, exists, mkdir')
import Node.FS.Perms (permsReadWrite)

import Tidy.Codegen

import Noodle.Id (toolkitR) as Id
import Noodle.Fn.Shape.Temperament (defaultAlgorithm) as Temperament
import Noodle.Text.ToCode (toCode)
import Noodle.Text.Code.Target (pureScript) as ToCode
import Noodle.Text.NdfFile (loadOrder, hasFailedLines, failedLines, codegen) as NdfFile
import Noodle.Text.NdfFile.Codegen as CG
import Noodle.Text.NdfFile.Parser (parser) as NdfFile
import Noodle.Text.NdfFile.FamilyDef (FamilyDef, chtv, i, o, qdefps, st) as FD
import Noodle.Text.NdfFile.FamilyDef.ProcessCode (ProcessCode(..)) as FD
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr, Options(..), withOptions) as CG
-- import Noodle.Text.NdfFile.Codegen as CG
import Noodle.Toolkit (Name) as Toolkit

import Example.Toolkit.Minimal.Repr (MinimalRepr)

import Hydra.Types (FnArg(..))
import Hydra.Repr.Wrap (WrapRepr, hydraGenOptions)

import Test.Spec.Util.Assertions (shouldEqual) as U


minimalGenOptions :: CG.Options MinimalRepr
minimalGenOptions = CG.Options
  { temperamentAlgorithm : Temperament.defaultAlgorithm
  , monadAt : { module_ : "Effect", type_ : "Effect" }
  , reprAt : { module_ : "Example.Toolkit.Minimal.Repr", type_ : "MinimalRepr" }
  , familyModuleName : CG.moduleName' modulePrefix $ Id.toolkitR "Test"
  , prepr : (Proxy :: _ MinimalRepr)
  , infoComment : Nothing
  , imports : unsafePartial $
    [ declImportAs "Data.String" [ importValue "length" ] "String"
    , declImport "Example.Toolkit.Minimal.Repr" [ importTypeAll "MinimalRepr" ]
    ]
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
        hydraToolkitText <- liftEffect $ readTextFile UTF8 "./src/Hydra/hydra.v0.3.ndf"
        let eParsedNdf = P.runParser hydraToolkitText NdfFile.parser
        case eParsedNdf of
          Left error -> fail $ show error
          Right parsedNdf ->
            if not $ NdfFile.hasFailedLines parsedNdf then do
              liftEffect $ Console.log $ show $ NdfFile.loadOrder parsedNdf
              let fileMap = NdfFile.codegen (Id.toolkitR "Hydra") customHydraGenOptions parsedNdf
              traverse_ testCodegenFile $ (Map.toUnfoldable fileMap :: Array (CG.FilePath /\ CG.FileContent))
            else
              fail $ "Failed to parse starting at:\n" <> (String.joinWith "\n" $ show <$> (Array.take 3 $ NdfFile.failedLines parsedNdf))


customHydraGenOptions :: CG.Options WrapRepr
customHydraGenOptions =
  CG.withOptions hydraGenOptions $ \opts -> opts
      { familyModuleName = CG.moduleName' modulePrefix $ Id.toolkitR "Hydra"
      , infoComment = Just $ \mbSource fgroup family ->
            case opts.infoComment of
              Just commentFn -> (commentFn mbSource fgroup family) <> "\n\n" <> "Toolkit : Hydra. File: ./src/Hydra/hydra.v0.3.ndf"
              Nothing -> "Toolkit : Hydra. File: ./src/Hydra/hydra.v0.3.ndf"
      }


modulePrefix = CG.ModulePrefix "Test.Files.CodeGenTest" :: CG.ModulePrefix


inputDir  = CG.GenRootPath "./test/Files/Input"  :: CG.GenRootPath
outputDir = CG.GenRootPath "./test/Files/Output" :: CG.GenRootPath


testSingleFamilyDef :: forall m repr. Bind m => MonadEffect m => MonadThrow _ m => CG.CodegenRepr repr => Toolkit.Name -> CG.Options repr -> FD.FamilyDef -> m Unit
testSingleFamilyDef tkName genOptions familyDef =
  let
    filePath = CG.moduleFile (CG.GenRootPath "") tkName familyDef
    fileContent = toCode (ToCode.pureScript) genOptions familyDef
  in testCodegenFile
     $ CG.FilePath filePath /\ CG.FileContent fileContent


testCodegenFile :: forall m. MonadEffect m => CG.FilePath /\ CG.FileContent -> m Unit
testCodegenFile (CG.FilePath filePath /\ CG.FileContent fileContent) = do
  let
    outputFilePath = unwrap outputDir <> filePath
    inputFilePath  = unwrap inputDir <> filePath
    outputDirectory = String.joinWith "/" $ Array.dropEnd 1 $ String.split (String.Pattern "/") outputFilePath
  liftEffect $ do
    outputDirectoryExists <- exists outputDirectory
    when (not outputDirectoryExists) $ mkdir' outputDirectory { mode : permsReadWrite, recursive : true }
    writeTextFile UTF8 outputFilePath fileContent
    sample <- readTextFile UTF8 inputFilePath
    fileContent `U.shouldEqual` sample
