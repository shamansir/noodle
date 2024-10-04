module Test.Spec.NdfCodegen where

import Prelude

import Data.String as String
import Data.Tuple.Nested ((/\), type (/\))

import Partial.Unsafe (unsafePartial)

import Type.Proxy (Proxy(..))

import Effect.Class (liftEffect)

import Test.Spec (Spec, describe, it, itOnly)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)

import Tidy.Codegen (declImportAs, importValue)

import Noodle.Fn.Shape.Temperament (defaultAlgorithm) as Temperament
import Noodle.Text.ToCode (toCode)
import Noodle.Text.ToCode (pureScript) as ToCode
import Noodle.Text.NdfFile.NodeDef as ND
import Noodle.Text.NdfFile.NodeDef.ProcessCode as ND
import Noodle.Text.NdfFile.NodeDef.Codegen as CG
import Noodle.Text.NdfFile.Types (NodeFamily(..))

import Test.MyToolkit.Repr (ISRepr)

import Test.Spec.Util.Assertions (shouldEqual) as U


samples :: Array (String /\ ND.NodeDef)
samples = []


spec :: Spec Unit
spec = do

    describe "NDF Codegen" $ do

      it "auto code definitions:" $ do
        (toCode (ToCode.pureScript) unit $ ND.Auto "") `U.shouldEqual` ""
        (toCode (ToCode.pureScript) unit $ ND.Auto "a") `U.shouldEqual` "a"
        (toCode (ToCode.pureScript) unit $ ND.Auto "outlet::<inlet1> + <inlet2>") `U.shouldEqual` """inlet1 <- Fn.receive in_inlet1
inlet2 <- Fn.receive in_inlet2
Fn.send out_outlet $ inlet1 + inlet2"""
        (toCode (ToCode.pureScript) unit $ ND.Auto "out::H.Start $ H.Solid { <r>, <g>, <b>, <a> }") `U.shouldEqual` """r <- Fn.receive in_r
g <- Fn.receive in_g
b <- Fn.receive in_b
a <- Fn.receive in_a
Fn.send out_out $ H.Start $ H.Solid { r, g, b, a }"""
        (toCode (ToCode.pureScript) unit $ ND.Auto "out::H.Start $ H.Solid { <r>, <g>, <b>, <a> };r::<r>;g::<g>;b::<b>;a::<a>") `U.shouldEqual` """r <- Fn.receive in_r
g <- Fn.receive in_g
b <- Fn.receive in_b
a <- Fn.receive in_a
Fn.send out_out $ H.Start $ H.Solid { r, g, b, a }
Fn.send out_r $ r
Fn.send out_g $ g
Fn.send out_b $ b
Fn.send out_a $ a"""
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