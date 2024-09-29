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
import Noodle.Text.NdfFile.Newtypes


import Tidy.Codegen
import Partial.Unsafe (unsafePartial)

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
            , defaultType : EncodedType "ISRepr", defaultValue : EncodedValue "None"
            , typeFor :
                unsafePartial $ \(EncodedType typeStr) ->
                  case typeStr of
                    "Int" -> typeCtor "Int"
                    "String" -> typeCtor "String"
                    _ -> typeCtor "ISRepr"
            , defaultFor :
                unsafePartial $ \(EncodedType typeStr) (EncodedValue valueStr) ->
                  case valueStr of
                    "" -> exprString ""
                    "0" -> exprInt 0
                    str -> exprString str
            }
          psModuleCode = toCode (ToCode.pureScript) (CG.Options genOptions) testNodeDef

        liftEffect $ writeTextFile UTF8 "./test/Files/Output/Temp.purs" psModuleCode
        psModuleCode `shouldEqual` ""
