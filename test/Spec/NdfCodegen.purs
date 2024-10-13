module Test.Spec.NdfCodegen where

import Prelude

import Data.String as String
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
import Node.FS.Sync (readTextFile, writeTextFile)

import Tidy.Codegen

import Noodle.Fn.Shape.Temperament (defaultAlgorithm) as Temperament
import Noodle.Text.ToCode (toCode)
import Noodle.Text.ToCode (pureScript) as ToCode
import Noodle.Text.NdfFile.NodeDef (family) as NodeDef
import Noodle.Text.NdfFile (loadDefinitions) as NdfFile
import Noodle.Text.NdfFile.Parser (parser) as NdfFile
import Noodle.Text.NdfFile.NodeDef (NodeDef, chtv, i, o, qdefps, st) as ND
import Noodle.Text.NdfFile.NodeDef.ProcessCode (ProcessCode(..)) as ND
import Noodle.Text.NdfFile.NodeDef.Codegen as CG
import Noodle.Text.NdfFile.Types (NodeFamily(..))

import Example.Toolkit.Minimal.Repr (MinimalRepr)

import Hydra.Types (FnArg(..))
import Hydra.Repr.Wrap (WrapRepr)

import Test.Spec.Util.Assertions (shouldEqual) as U


minimalGenOptions :: CG.Options MinimalRepr
minimalGenOptions = CG.Options
  { temperamentAlgorithm : Temperament.defaultAlgorithm
  , monadAt : { module_ : "Effect", type_ : "Effect" }
  , nodeModuleName
  , prepr : (Proxy :: _ MinimalRepr)
  , imports : unsafePartial $
    [ declImportAs "Data.String" [ importValue "length" ] "String"
    , declImport "Example.Toolkit.Minimal.Repr" [ importTypeAll "MinimalRepr" ]
    ]
  }


hydraGenOptions :: CG.Options WrapRepr
hydraGenOptions = CG.Options
  { temperamentAlgorithm : Temperament.defaultAlgorithm
  , monadAt : { module_ : "Effect", type_ : "Effect" }
  , nodeModuleName
  , prepr : (Proxy :: _ WrapRepr)
  , imports : unsafePartial $
    [ declImportAs "Hydra.Types" [ ] "H"
    , declImport "Hydra.WrapRepr" [ ]
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

        testNodeDefCodegen minimalGenOptions (familyUp <<< NodeDef.family) testNodeDef

      it "properly generates Hydra Toolkit" $ do
        hydraToolkitText <- liftEffect $ readTextFile UTF8 "./src/Hydra/hydra.v0.2.ndf"
        let eParsedNdf = P.runParser hydraToolkitText NdfFile.parser
        case eParsedNdf of
          Left error -> fail $ show error
          Right parsedNdf -> do
            let definitions = NdfFile.loadDefinitions parsedNdf
            traverse_ ( testNodeDefCodegen hydraGenOptions $ \nodeDef -> "Hydra/" <> (familyUp $ NodeDef.family nodeDef)) definitions


familyUp :: NodeFamily -> String
familyUp (NodeFamily family) = String.toUpper (String.take 1 family) <> String.drop 1 family


nodeModuleName :: NodeFamily -> String
nodeModuleName family =
  "Test.Files.CodeGenTest." <> familyUp family


testNodeDefCodegen :: forall m repr. Bind m => MonadEffect m => MonadThrow _ m => CG.CodegenRepr repr => CG.Options repr -> (ND.NodeDef -> String) -> ND.NodeDef -> m Unit
testNodeDefCodegen genOptions toFileName  nodeDef = do
  let
    psModuleCode = toCode (ToCode.pureScript) genOptions nodeDef
    samplePath = "./test/Files/Input/"  <> toFileName nodeDef <> ".purs"
    targetPath = "./test/Files/Output/" <> toFileName nodeDef <> ".purs"
  liftEffect $ writeTextFile UTF8 targetPath psModuleCode
  sample <- liftEffect $ readTextFile UTF8 samplePath
  psModuleCode `U.shouldEqual` sample
