module Test.Spec.NdfCodegen where

import Prelude

import Control.Monad.Writer (tell)
import Partial.Unsafe (unsafePartial)

import Effect.Class (liftEffect)

import PureScript.CST.Types

import Tidy.Codegen
import Tidy.Codegen.Monad

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Path (FilePath, extname, basenameWithoutExt)

import Test.Spec (Spec, describe, it, itOnly)
import Test.Spec.Assertions (shouldEqual)

exampleModule :: Module Void
exampleModule = unsafePartial $ codegenModule "MyModule.UsingCodegen" do
  importOpen "Prelude"
  tell
    [ declSignature "_concat" $ typeApp (typeCtor "NId.Family") [ typeString "concat" ]
    , declValue "_concat" [] $ exprCtor "NId.Family"

    , declType "Inlets" []
        $ typeKinded
          (typeParens $ typeOp
            (typeApp (typeCtor "I") [ typeString "left", typeCtor "Hot", typeCtor "String" ])
            [ binaryOp ":>" (typeApp (typeCtor "I") [ typeString "right", typeCtor "Hot", typeCtor "String" ])
            , binaryOp ":>" (typeCtor "TNil")
            ]
          )
        $ typeCtor "Noodle.Inlets"

    , declType "Outlets" []
        $ typeKinded
        ( typeParens $ typeOp
          (typeApp (typeCtor "O") [ typeString "out", typeCtor "String" ])
          [ binaryOp ":>" (typeApp (typeCtor "O") [ typeString "len", typeCtor "Int" ])
          , binaryOp ":>" (typeCtor "TNil")
          ]
        )
        $ typeCtor "Noodle.Outlets"

    , declType "InletsRow" []
        $ typeRow
          [ "left" /\ typeCtor "String"
          , "right" /\ typeCtor "String"
          ]
        Nothing

    , declType "OutletsRow" []
        $ typeRow
          [ "out" /\ typeCtor "String"
          , "len" /\ typeCtor "Int"
          ]
        Nothing

    , declType "Shape" []
        $ typeApp (typeCtor "Noodle.Shape")
            [ typeCtor "Inlets", typeCtor "Outlets" ]

    , declType "Process" []
        $ typeApp (typeCtor "Noodle.Process")
            [ typeCtor "Unit"
            , typeCtor "InletsRow"
            , typeCtor "OutletsRow"
            , typeCtor "ISRepr"
            , typeCtor "Effect"
            ]

    , declType "Node" []
        $ typeApp (typeCtor "Noodle.Node")
            [ typeString "concat"
            , typeCtor "Unit"
            , typeCtor "InletsRow"
            , typeCtor "OutletsRow"
            , typeCtor "ISRepr"
            , typeCtor "Effect"
            ]

    , declType "Family" []
        $ typeApp (typeCtor "Noodle.F")
            [ typeString "concat"
            , typeCtor "Unit"
            , typeCtor "InletsRow"
            , typeCtor "OutletsRow"
            , typeCtor "ISRepr"
            , typeCtor "Effect"
            ]

    , declSignature "defaultI" $ typeApp (typeCtor "Record") [ typeCtor "InletsRow" ]
    , declValue "defaultI" []
        $ exprRecord
          [ "left" /\ exprString ""
          , "right" /\ exprString ""
          ]

    , declSignature "defaultO" $ typeApp (typeCtor "Record") [ typeCtor "InletsRow" ]
    , declValue "defaultO" []
        $ exprRecord
          [ "out" /\ exprString ""
          , "len" /\ exprInt 0
          ]

    , declValue "left_in" []
        $ exprTyped
          (exprCtor "Noodle.Inlet")
          (typeApp
            typeWildcard
            [ typeString "left" ]
          )

    , declValue "right_in" []
        $ exprTyped
          (exprCtor "Noodle.Inlet")
          (typeApp
            typeWildcard
            [ typeString "right" ]
          )

    , declValue "out_out" []
        $ exprTyped
          (exprCtor "Noodle.Outlet")
          (typeApp
            typeWildcard
            [ typeString "out" ]
          )

    , declValue "len_out" []
        $ exprTyped
          (exprCtor "Noodle.Outlet")
          (typeApp
            typeWildcard
            [ typeString "len" ]
          )

    , declSignature "family" $ typeCtor "Family"
    , declValue "family" []
        $ exprApp
          (exprIdent "Family.make")
          [ exprIdent "_concat"
          , exprIdent "unit"
          , exprParens (exprTyped (exprCtor "Noodle.Shape") (typeCtor "Shape"))
          , exprIdent "defaultI"
          , exprIdent "defaultO"
          , exprIdent "concatP"
          ]

    , declSignature "makeNode" $ typeApp (typeCtor "Effect") [ typeCtor "Node" ]
    , declValue "makeNode" []
        $ exprApp
          (exprIdent "Family.spawn")
          [ exprIdent "family"
          ]

    , declSignature "concatP" $ typeCtor "Process"
    , declValue "concatP" []
        $ exprDo
          [ doBind (binderVar "left") $ exprApp (exprIdent "Fn.receive") [ exprIdent "left_in" ]
          , doBind (binderVar "right") $ exprApp (exprIdent "Fn.receive") [ exprIdent "right_in" ]
          , doLet
            [ letBinder
                (binderVar "concatenated")
                (exprOp (exprIdent "left")
                  [ binaryOp "<>" $ exprIdent "right" ]
                )
            ]
          , doDiscard $ exprApp (exprIdent "Fn.send") [ exprIdent "out_out", exprIdent "concatenated" ]
          , doDiscard $ exprOp
                (exprApp (exprIdent "Fn.send") [ exprIdent "len_out" ])
                [ binaryOp "$" $ exprApp (exprIdent "String.length") [ exprIdent "concatenated" ] ]
          ]
          (exprApp (exprIdent "pure") [ exprIdent "unit" ])

    ]


spec :: Spec Unit
spec = do

    describe "NDF Codegen" $ do

      itOnly "should compile to the expected code" $ do
        liftEffect $ writeTextFile UTF8 "./test/Files/Output/Temp.purs" $ printModule exampleModule
        printModule exampleModule `shouldEqual` ""
