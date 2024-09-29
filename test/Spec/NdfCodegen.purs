module Test.Spec.NdfCodegen where

import Prelude

import Effect.Class (liftEffect)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Path (FilePath, extname, basenameWithoutExt)

import Test.Spec (Spec, describe, it, itOnly)
import Test.Spec.Assertions (shouldEqual)

import Noodle.Fn.Shape.Temperament (defaultAlgorithm) as Temperament
import Noodle.Text.ToCode (toCode)
import Noodle.Text.ToCode (pureScript) as ToCode
import Noodle.Text.NdfFile.NodeDef as ND
import Noodle.Text.NdfFile.NodeDef.Codegen as CG



spec :: Spec Unit
spec = do

    describe "NDF Codegen" $ do

      it "should compile to the expected code" $ do
        let
          testNodeDef = ND.qdef
            { group : "all", family : "concat"
            , inputs :
              [ ND.i $ ND.chtv "left" "String" ""
              , ND.i $ ND.chtv "right" "String" ""
              ]
            , outputs :
              [ ND.o $ ND.chtv "out" "String" ""
              , ND.o $ ND.chtv "len" "Int" "0"
              ]
            }
          genOptions =
            { temperamentAlgorithm : Temperament.defaultAlgorithm
            , monadModule : "Effect", monadType : "Effect"
            , reprModule : "Test.MyToolkit.Repr", reprType : "ISRepr"
            , defaultType : "ISRepr", defaultValue : "None"
            }
          psModuleCode = toCode (ToCode.pureScript) (CG.Options genOptions) testNodeDef

        liftEffect $ writeTextFile UTF8 "./test/Files/Output/Temp.purs" psModuleCode
        psModuleCode `shouldEqual` ""
