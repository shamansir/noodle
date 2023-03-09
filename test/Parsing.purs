module Test.Parsing where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.String.CodeUnits as String
import Data.String (joinWith, split, Pattern(..)) as String
import Data.Array as Array

import Control.Monad.Error.Class (class MonadThrow)

import Effect (Effect)
import Effect.Aff (launchAff, launchAff_, runAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Text.Parsing.Parser as P
import Text.Parsing.Parser.String as P
import Text.Parsing.Parser.Token as P
import Text.Parsing.Parser.Combinators (choice, sepBy) as P

import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile, appendTextFile)

import Noodle.Text.QuickDef as QD
import Noodle.Text.QuickDefParser as QDP
import Noodle.Text.ToolkitGen as QTG


in_file_path = "./hydra.fn.clean.list"
out_file_path = "./test/hydra.toolkit.prepurs"
out_file_path_sample = "./test/hydra.toolkit.prepurs.sample"


{-
loadFile :: forall m. MonadEffect m => m Unit
loadFile =
    liftEffect
        $ runAff_ (\_ -> pure unit)
        $ readTextFile UTF8 "./hydra.fn.clean.list"
-}


parses :: forall (s :: Type) (m :: Type -> Type) (a :: Type). MonadThrow Error m => Show a => Eq a => s -> a -> P.ParserT s Identity a -> m Unit
parses string expected parser =
  case P.runParser string parser of
    Right result ->
      result `shouldEqual` expected
    Left error ->
      fail $ show error


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do

  describe "Toolkit Defs to code" $ do

    it "parsing works" $
      parses "x" 'x' $ P.char 'x'

    it "parsing with sep works" $ do
      let
        arrowSep = do
          _ <- Array.many P.space
          _ <- P.choice [ P.string "->", String.singleton <$> P.char '→' ]
          _ <- Array.many P.space
          pure unit
        qParser = Array.fromFoldable <$> P.char '?' `P.sepBy` arrowSep

      parses "x" [] qParser
      parses "?" ['?'] qParser
      parses "? -> ?" [ '?', '?' ] qParser
      parses "?->?" [ '?', '?' ] qParser
      parses "?->? ->?" [ '?', '?', '?' ] qParser
      parses "? -> ? -> ?" [ '?', '?', '?' ] qParser

    it "parsing one function with no arguments works" $ do
      parses
        "test : foo :: => Result"
        (QD.qfm "test" "foo" [] "Result")
        QDP.fnParser
      parses
        "test : foo :: <> => Result"
        (QD.qfm "test" "foo" [] "Result")
        QDP.fnParser

    it "parsing one function with an unkwown argument works" $
      parses
        "test : foo :: <?> => Result"
        (QD.qfm' "test" "foo" [ Nothing ] "Result")
        QDP.fnParser

    it "parsing one function with two unkwown arguments works" $ do
      parses
        "test : foo :: <?->?> => Result"
        (QD.qfm' "test" "foo" [ Nothing, Nothing ] "Result")
        QDP.fnParser
      parses
        "test : foo :: <? -> ?> => Result"
        (QD.qfm' "test" "foo" [ Nothing, Nothing ] "Result")
        QDP.fnParser
      parses
        "test : foo :: <? ->?> => Result"
        (QD.qfm' "test" "foo" [ Nothing, Nothing ] "Result")
        QDP.fnParser

    it "parsing one function with arguments works" $ do
      parses
        "test : foo :: <bar->?> => Result"
        (QD.qfm' "test" "foo" [ Just (QD.qchan "bar"), Nothing ] "Result")
        QDP.fnParser
      parses
        "test : foo :: <? -> bar> => Result"
        (QD.qfm' "test" "foo" [ Nothing, Just (QD.qchan "bar") ] "Result")
        QDP.fnParser
      parses
        "test : foo :: <bar:Number -> buz:Number {20}> => Result"
        (QD.qfm "test" "foo" [ QD.qchant "bar" "Number", QD.qchantd "buz" "Number" "20" ] "Result")
        QDP.fnParser
      parses
        "modulate : modulateRepeat :: <what:Texture -> with:Texture -> repeatX:Value {3} -> repeatY:Value {3} -> offsetX:Value {0.5} -> offsetY:Value {0.5}> => Texture"
        (QD.qfm "modulate" "modulateRepeat"
          [ QD.qchant "what" "Texture"
          , QD.qchant "with" "Texture"
          , QD.qchantd "repeatX" "Value" "3"
          , QD.qchantd "repeatY" "Value" "3"
          , QD.qchantd "offsetX" "Value" "0.5"
          , QD.qchantd "offsetY" "Value" "0.5"
          ]
        "Texture")
        QDP.fnParser
      parses
        "synth : render :: <from:From {All}> => Unit"
        (QD.qfm "synth" "render"
          [ QD.qchantd "from" "From" "All"
          ]
        "Unit")
        QDP.fnParser
      parses
        "synth : update :: <fn:UpdateFn> => Unit"
        (QD.qfm "synth" "update"
          [ QD.qchant "fn" "UpdateFn"
          ]
        "Unit")
        QDP.fnParser

    it "parsing one function with several outputs works" $ do
      parses
        "synth : update :: <fn:UpdateFn> => <foo:UpdateFn {20} -> bar {5} -> buz -> bzr:Unit {Test}>"
        (QD.qfmo "synth" "update"
          [ QD.qchant "fn" "UpdateFn"
          ]
          [ QD.qchantd "foo" "UpdateFn" "20"
          , QD.qchand "bar" "5"
          , QD.qchan "buz"
          , QD.qchantd "bzr" "Unit" "Test"
          ])
        QDP.fnParser

    it "parses file" $ do
        file <- readTextFile UTF8 in_file_path
        let parseResult = P.runParser file QDP.familyListParser
        case parseResult of
          Right result ->
            Array.length result `shouldEqual` 84
          Left error ->
            fail $ show error

    it "converts properly" $ do
        file <- readTextFile UTF8 in_file_path
        let parseResult = P.runParser file QDP.familyListParser
        sampleContent <- readTextFile UTF8 out_file_path_sample
        case parseResult of
          Right familiesList -> do
            let genSumType = QTG.genSumType "hydra" familiesList
            let sepImports = QTG.genSeparateImports "hydra" familiesList
            let sepNodesTypes = QTG.genSeparateFamilyTypes true "hydra" familiesList
            let sepImpls = QTG.genSeparateFamilyImpls true "hydra" familiesList
            let sepTypeDef = QTG.genTypeDefSeparate true "hydra" familiesList
            let familyModules = String.joinWith "\n\n{- MODULE -}\n\n" (QTG.genFamilyModule "hydra" <$> familiesList)
            let sepImpls' = QTG.genSeparateFamilyImpls false "hydra" familiesList
            let sepTypeDef' = QTG.genTypeDefSeparate false "hydra" familiesList
            let inlineTypeDef = QTG.genTypeDefInline "hydra" familiesList
            let toolkitDef = QTG.genToolkitDef "hydra" familiesList
            let fileContent =
                    String.joinWith "\n\n\n"
                      [ genSumType
                      , sepImports
                      , sepNodesTypes
                      , sepImpls
                      , sepTypeDef
                      , sepImpls'
                      , sepTypeDef'
                      , inlineTypeDef
                      , toolkitDef
                      , familyModules
                      ]
            writeTextFile UTF8 out_file_path fileContent
            _ <- Array.zipWithA shouldEqual
                      (String.split (String.Pattern "\n") fileContent)
                      (String.split (String.Pattern "\n") sampleContent)
            pure unit
            -- fileContent `shouldEqual` sampleContent
          Left error ->
            fail $ show error