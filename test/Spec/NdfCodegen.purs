module Test.Spec.NdfCodegen where

import Prelude

import Control.Monad.Writer (tell)
import Partial.Unsafe (unsafePartial)

import Effect.Class (liftEffect)

import PureScript.CST.Types

import Tidy.Codegen hiding (importType, importTypeOp, importValue, importTypeAll)
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
  effect <- importFrom "Effect" $ importType "Effect"
  listOp <- importFrom "Type.Data.List" $ importTypeOp ":>"
  listTnil <- importFrom "Type.Data.List.Extra" $ importType "TNil"
  stringLen <- importFrom "Data.String" $ importValue "String.length"
  familyIdCtor <- importFrom "Noodle.Id" $ importTypeAll "NId.Family"
  temper <- importFrom "Noodle.Fn.Shape.Temperament" $
    { hot : importType "Hot"
    , cold : importType "Cold"
    }
  shape <- importFrom "Noodle.Fn.Shape" $
    { i : importType "I"
    , o : importType "O"
    }
  shapeN <- importFrom "Noodle.Fn.Shape" $
    { shape : importTypeAll "Noodle.Shape"
    , inlets : importType "Noodle.Inlets"
    , outlets : importType "Noodle.Outlets"
    , inlet : importTypeAll "Noodle.Inlet"
    , outlet : importTypeAll "Noodle.Outlet"
    }
  processN <- importFrom "Noodle.Fn.Process" $ importType "Noodle.Process"
  processFn <- importFrom "Noodle.Fn.Process" $
    { receive : importValue "Fn.receive"
    , send : importValue "Fn.send"
    }
  nodeCtor <- importFrom "Noodle.Node" $ importType "Noodle.Node"
  tkFamilyN <- importFrom "Noodle.Toolkit.Family" $ importType "Noodle.Family"
  tkFamilyF <- importFrom "Noodle.Toolkit.Family" $
    { make : importValue "Family.make"
    , spawn : importValue "Family.spawn"
    }
  tkF <- importFrom "Noodle.Toolkit.Families" $ importType "Noodle.F"
  reprC <- importFrom "Test.MyToolkit.Repr" $ importType "ISRepr"

  tell
    [ declSignature "_concat" $ typeApp (typeCtor familyIdCtor) [ typeString "concat" ]
    , declValue "_concat" [] $ exprCtor familyIdCtor

    , declType "Inlets" []
        $ typeKinded
          (typeParens $ typeOp
            (typeApp (typeCtor shape.i) [ typeString "left", typeCtor temper.hot, typeCtor "String" ])
            [ binaryOp listOp (typeApp (typeCtor shape.i) [ typeString "right", typeCtor temper.hot, typeCtor "String" ])
            , binaryOp listOp (typeCtor listTnil)
            ]
          )
        $ typeCtor shapeN.inlets

    , declType "Outlets" []
        $ typeKinded
        ( typeParens $ typeOp
          (typeApp (typeCtor shape.o) [ typeString "out", typeCtor "String" ])
          [ binaryOp listOp (typeApp (typeCtor shape.o) [ typeString "len", typeCtor "Int" ])
          , binaryOp listOp (typeCtor listTnil)
          ]
        )
        $ typeCtor shapeN.outlets

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
        $ typeApp (typeCtor shapeN.shape)
            [ typeCtor "Inlets", typeCtor "Outlets" ]

    , declType "Process" []
        $ typeApp (typeCtor processN)
            [ typeCtor "Unit"
            , typeCtor "InletsRow"
            , typeCtor "OutletsRow"
            , typeCtor reprC
            , typeCtor effect
            ]

    , declType "Node" []
        $ typeApp (typeCtor nodeCtor)
            [ typeString "concat"
            , typeCtor "Unit"
            , typeCtor "InletsRow"
            , typeCtor "OutletsRow"
            , typeCtor reprC
            , typeCtor effect
            ]

    , declType "Family" []
        $ typeApp (typeCtor tkFamilyN)
            [ typeString "concat"
            , typeCtor "Unit"
            , typeCtor "InletsRow"
            , typeCtor "OutletsRow"
            , typeCtor reprC
            , typeCtor effect
            ]

    , declType "F" []
        $ typeApp (typeCtor tkF)
            [ typeString "concat"
            , typeCtor "Unit"
            , typeCtor "InletsRow"
            , typeCtor "OutletsRow"
            , typeCtor reprC
            , typeCtor effect
            ]

    , declSignature "defaultI" $ typeApp (typeCtor "Record") [ typeCtor "InletsRow" ]
    , declValue "defaultI" []
        $ exprRecord
          [ "left" /\ exprString ""
          , "right" /\ exprString ""
          ]

    , declSignature "defaultO" $ typeApp (typeCtor "Record") [ typeCtor "OutletsRow" ]
    , declValue "defaultO" []
        $ exprRecord
          [ "out" /\ exprString ""
          , "len" /\ exprInt 0
          ]

    , declValue "left_in" []
        $ exprTyped
          (exprCtor shapeN.inlet)
          (typeApp
            typeWildcard
            [ typeString "left" ]
          )

    , declValue "right_in" []
        $ exprTyped
          (exprCtor shapeN.inlet)
          (typeApp
            typeWildcard
            [ typeString "right" ]
          )

    , declValue "out_out" []
        $ exprTyped
          (exprCtor shapeN.outlet)
          (typeApp
            typeWildcard
            [ typeString "out" ]
          )

    , declValue "len_out" []
        $ exprTyped
          (exprCtor shapeN.outlet)
          (typeApp
            typeWildcard
            [ typeString "len" ]
          )

    , declSignature "family" $ typeCtor "Family"
    , declValue "family" []
        $ exprApp
          (exprIdent tkFamilyF.make)
          [ exprIdent "_concat"
          , exprIdent "unit"
          , exprParens (exprTyped (exprCtor "Noodle.Shape") (typeCtor "Shape"))
          , exprIdent "defaultI"
          , exprIdent "defaultO"
          , exprIdent "concatP"
          ]

    , declSignature "makeNode" $ typeApp (typeCtor effect) [ typeCtor "Node" ]
    , declValue "makeNode" []
        $ exprApp
          (exprIdent tkFamilyF.spawn)
          [ exprIdent "family"
          ]

    , declSignature "concatP" $ typeCtor "Process"
    , declValue "concatP" []
        $ exprDo
          [ doBind (binderVar "left") $ exprApp (exprIdent processFn.receive) [ exprIdent "left_in" ]
          , doBind (binderVar "right") $ exprApp (exprIdent processFn.receive) [ exprIdent "right_in" ]
          , doLet
            [ letBinder
                (binderVar "concatenated")
                (exprOp (exprIdent "left")
                  [ binaryOp "<>" $ exprIdent "right" ]
                )
            ]
          , doDiscard $ exprApp (exprIdent processFn.send) [ exprIdent "out_out", exprIdent "concatenated" ]
          , doDiscard $ exprOp
                (exprApp (exprIdent processFn.send) [ exprIdent "len_out" ])
                [ binaryOp "$" $ exprApp (exprIdent stringLen) [ exprIdent "concatenated" ] ]
          ]
          (exprApp (exprIdent "pure") [ exprIdent "unit" ])

    ]


spec :: Spec Unit
spec = do

    describe "NDF Codegen" $ do

      itOnly "should compile to the expected code" $ do
        liftEffect $ writeTextFile UTF8 "./test/Files/Output/Temp.purs" $ printModule exampleModule
        printModule exampleModule `shouldEqual` ""
