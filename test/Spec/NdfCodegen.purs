module Test.Spec.NdfCodegen where

import Prelude

import Data.String as String
import Data.Tuple.Nested ((/\), type (/\))

import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

import Type.Proxy (Proxy(..))

import Effect.Class (liftEffect)

import Test.Spec (Spec, describe, it, itOnly)
import Test.Spec.Assertions (shouldEqual)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Path (FilePath, extname, basenameWithoutExt)

import Tidy.Codegen
import Tidy.Codegen.Monad (Codegen, importFrom, class ToImportFrom)
import Tidy.Codegen.Class (class ToModuleName)

import Noodle.Fn.Shape.Temperament (defaultAlgorithm) as Temperament
import Noodle.Text.ToCode (toCode)
import Noodle.Text.ToCode (pureScript) as ToCode
import Noodle.Text.NdfFile.NodeDef as ND
import Noodle.Text.NdfFile.NodeDef.Codegen as CG
import Noodle.Text.NdfFile.Newtypes

import Test.MyToolkit.Repr (ISRepr)

import Test.Spec.Util.Assertions (shouldEqual) as U


samples :: Array (String /\ ND.NodeDef)
samples = []


spec :: Spec Unit
spec = do

    describe "NDF Codegen" $ do

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
            , process : """do
  left <- Fn.receive left_in
  right <- Fn.receive right_in
  let concatenated = left <> right
  Fn.send out_out concatenated
  Fn.send len_out $ String.length concatenated"""
            }
          familyUp (NodeFamily family) = String.toUpper (String.take 1 family) <> String.drop 1 family
          nodeModuleName family =
            "Test.Files.CodeGenTest." <> familyUp family
          genOptions =
            { temperamentAlgorithm : Temperament.defaultAlgorithm
            , monadAt : { module_ : "Effect", type_ : "Effect" }
            , nodeModuleName
            , prepr : (Proxy :: _ ISRepr)
            , imports : unsafePartial $
              [ declImportAs "Data.String" [ importValue "length" ] "String"
              ]
            }
          psModuleCode = toCode (ToCode.pureScript) (CG.Options genOptions) testNodeDef
          samplePath = "./test/Files/Input/"  <> familyUp (NodeFamily "concat") <> ".purs"
          targetPath = "./test/Files/Output/" <> familyUp (NodeFamily "concat") <> ".purs"

        sample <- liftEffect $ readTextFile UTF8 samplePath

        liftEffect $ writeTextFile UTF8 targetPath psModuleCode
        psModuleCode `U.shouldEqual` sample