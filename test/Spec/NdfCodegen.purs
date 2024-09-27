module Test.Spec.NdfCodegen where

import Prelude

import Control.Monad.Writer (tell)
import Partial.Unsafe (unsafePartial)

import Effect.Class (liftEffect)

import PureScript.CST.Types

import Tidy.Codegen
import Tidy.Codegen.Monad

import Data.Maybe (Maybe(..))

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Path (FilePath, extname, basenameWithoutExt)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

exampleModule :: Module Void
exampleModule = unsafePartial $ codegenModule "Data.Maybe" do
  importOpen "Prelude"
  tell
    [ declData "Maybe" [ typeVar "a" ]
        [ dataCtor "Nothing" []
        , dataCtor "Just" [ typeVar "a" ]
        ]

    , declDerive Nothing [] "Functor" [ typeCtor "Maybe" ]

    , declSignature "maybe" do
        typeForall [ typeVar "a", typeVar "b" ] do
          typeArrow
            [ typeVar "b"
            , typeArrow [ typeVar "a" ] (typeVar "b")
            , typeApp (typeCtor "Maybe") [ typeVar "a" ]
            ]
            (typeVar "b")

    , declValue "maybe" [ binderVar "nothing", binderVar "just" ] do
        exprCase [ exprSection ]
          [ caseBranch [ binderCtor "Just" [ binderVar "a" ] ] do
              exprApp (exprIdent "just") [ exprIdent "a" ]
          , caseBranch [ binderCtor "Nothing" [] ] do
              exprIdent "nothing"
          ]
    ]


spec :: Spec Unit
spec = do

    describe "performing functions" $ do

      it "should compile to the expected code" $ do
        liftEffect $ writeTextFile UTF8 "./test/Files/Output/Temp.purs" $ printModule exampleModule
        printModule exampleModule `shouldEqual` ""
