module Test.Spec.NdfCodegen where

import Prelude

import Partial.Unsafe (unsafePartial)

import Type.Proxy (Proxy(..))

import Effect.Class (liftEffect)

import Test.Spec (Spec, describe, it, itOnly)
import Test.Spec.Assertions (shouldEqual)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Path (FilePath, extname, basenameWithoutExt)

import Tidy.Codegen

import Noodle.Fn.Shape.Temperament (defaultAlgorithm) as Temperament
import Noodle.Text.ToCode (toCode)
import Noodle.Text.ToCode (pureScript) as ToCode
import Noodle.Text.NdfFile.NodeDef as ND
import Noodle.Text.NdfFile.NodeDef.Codegen as CG
import Noodle.Text.NdfFile.Newtypes

import Test.MyToolkit.Repr (ISRepr)

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
            , prepr : (Proxy :: _ ISRepr)
            }
          psModuleCode = toCode (ToCode.pureScript) (CG.Options genOptions) testNodeDef

        liftEffect $ writeTextFile UTF8 "./test/Files/Output/Temp.purs" psModuleCode
        psModuleCode `shouldEqual` ""
