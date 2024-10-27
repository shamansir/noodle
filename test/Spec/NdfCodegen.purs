module Test.Spec.NdfCodegen where

import Prelude

import Data.String as String
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Traversable (traverse_)

import Control.Monad.Error.Class (class MonadThrow)

import Partial.Unsafe (unsafePartial)

import Type.Proxy (Proxy(..))

import Effect.Class (class MonadEffect, liftEffect)

import Parsing (runParser) as P

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile, mkdir, exists)

import Tidy.Codegen

import Noodle.Fn.Shape.Temperament (defaultAlgorithm) as Temperament
import Noodle.Text.ToCode (toCode)
import Noodle.Text.Code.Target (pureScript) as ToCode
import Noodle.Text.NdfFile.NodeDef (family, group) as NodeDef
import Noodle.Text.NdfFile (loadDefinitions) as NdfFile
import Noodle.Text.NdfFile.Parser (parser) as NdfFile
import Noodle.Text.NdfFile.NodeDef (NodeDef, chtv, i, o, qdefps, st) as ND
import Noodle.Text.NdfFile.NodeDef.ProcessCode (ProcessCode(..)) as ND
import Noodle.Text.NdfFile.NodeDef.Codegen as CG
import Noodle.Text.NdfFile.Types (NodeFamily(..), FamilyGroup(..))
import Noodle.Toolkit (Name) as Toolkit

import Example.Toolkit.Minimal.Repr (MinimalRepr)

import Hydra.Types (FnArg(..))
import Hydra.Repr.Wrap (WrapRepr, hydraGenOptions)

import Test.Spec.Util.Assertions (shouldEqual) as U


minimalGenOptions :: CG.Options MinimalRepr
minimalGenOptions = CG.Options
  { temperamentAlgorithm : Temperament.defaultAlgorithm
  , monadAt : { module_ : "Effect", type_ : "Effect" }
  , nodeModuleName : moduleName "Test"
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
        (toCode (ToCode.pureScript) unit $ ND.Auto "outlet::<inlet1> + <inlet2>") `U.shouldEqual` """do
  inlet1 <- Fn.receive _in_inlet1
  inlet2 <- Fn.receive _in_inlet2
  Fn.send _out_outlet $ inlet1 + inlet2"""
        (toCode (ToCode.pureScript) unit $ ND.Auto "out::H.Start $ H.Solid { <r>, <g>, <b>, <a> }") `U.shouldEqual` """do
  r <- Fn.receive _in_r
  g <- Fn.receive _in_g
  b <- Fn.receive _in_b
  a <- Fn.receive _in_a
  Fn.send _out_out $ H.Start $ H.Solid { r, g, b, a }"""
        (toCode (ToCode.pureScript) unit $ ND.Auto "out::H.Start $ H.Solid { <r>, <g>, <b>, <a> };r::<r>;g::<g>;b::<b>;a::<a>") `U.shouldEqual` """do
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
          testNodeDef = ND.qdefps
            { group : "all", family : "concat"
            , inputs :
              [ ND.i $ ND.chtv "left" "String" ""
              , ND.i $ ND.chtv "right" "String" ""
              ]
            , outputs :
              [ ND.o $ ND.chtv "out" "String" ""
              , ND.o $ ND.chtv "len" "Int" "0"
              ]
            , state : ND.st "Unit" "unit"
            , process : ND.Raw """do
  left <- Fn.receive _in_left
  right <- Fn.receive _in_right
  let concatenated = left <> right
  Fn.send _out_out concatenated
  Fn.send _out_len $ String.length concatenated"""
            }

        testNodeDefCodegen "Test" minimalGenOptions testNodeDef

      it "properly generates Hydra Toolkit" $ do
        hydraToolkitText <- liftEffect $ readTextFile UTF8 "./src/Hydra/hydra.v0.3.ndf"
        let eParsedNdf = P.runParser hydraToolkitText NdfFile.parser
        case eParsedNdf of
          Left error -> fail $ show error
          Right parsedNdf -> do
            let definitions = NdfFile.loadDefinitions parsedNdf
            traverse_ ( testNodeDefCodegen "Hydra" customHydraGenOptions ) definitions


customHydraGenOptions :: CG.Options WrapRepr
customHydraGenOptions =
  case hydraGenOptions of
    (CG.Options hgRec) -> CG.Options $ hgRec
      { nodeModuleName = moduleName "Hydra"
      , infoComment = Just $ \mbSource fgroup family ->
            case hgRec.infoComment of
              Just commentFn -> (commentFn mbSource fgroup family) <> "\n\n" <> "Toolkit : Hydra. File: ./src/Hydra/hydra.v0.3.ndf"
              Nothing -> "Toolkit : Hydra. File: ./src/Hydra/hydra.v0.3.ndf"
      }


moduleName :: Toolkit.Name -> FamilyGroup -> NodeFamily -> String
moduleName tkName group family =
  "Test.Files.CodeGenTest." <> tkName <> "." <> CG.groupPascalCase group <> "." <> CG.familyPascalCase family


toolkitPath :: Toolkit.Name -> String
toolkitPath = identity


modulePath :: Toolkit.Name -> ND.NodeDef -> String
modulePath tkName nodeDef =
  tkName <> "/" <> (CG.groupPascalCase $ NodeDef.group nodeDef)


moduleFile :: Toolkit.Name -> ND.NodeDef -> String
moduleFile tkName nodeDef =
  modulePath tkName nodeDef <> "/" <> (CG.familyPascalCase $ NodeDef.family nodeDef) <> ".purs"


inputDir = "./test/Files/Input/" :: String
outputDir = "./test/Files/Output/" :: String


testNodeDefCodegen :: forall m repr. Bind m => MonadEffect m => MonadThrow _ m => CG.CodegenRepr repr => Toolkit.Name -> CG.Options repr -> ND.NodeDef -> m Unit
testNodeDefCodegen tkName genOptions nodeDef = do
  let
    psModuleCode = toCode (ToCode.pureScript) genOptions nodeDef
    moduleSampleFile = inputDir <> moduleFile tkName nodeDef
    moduleTargetPath = outputDir <> modulePath tkName nodeDef
    moduleTargetFile = outputDir <> moduleFile tkName nodeDef
    toolkitTargetPath = outputDir <> toolkitPath tkName
  liftEffect $ do
    tkDirExists <- exists toolkitTargetPath
    when (not tkDirExists) $ mkdir toolkitTargetPath
    moduleDirExists <- exists moduleTargetPath
    when (not moduleDirExists) $ mkdir moduleTargetPath
    writeTextFile UTF8 moduleTargetFile psModuleCode
    sample <- readTextFile UTF8 moduleSampleFile
    psModuleCode `U.shouldEqual` sample
