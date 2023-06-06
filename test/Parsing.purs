module Test.Parsing where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.String.CodeUnits as String
import Data.String (joinWith, split, Pattern(..)) as String
import Data.Array as Array
import Data.Tuple.Nested ((/\))
import Data.Traversable (for_)

import Control.Monad.Error.Class (class MonadThrow)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (launchAff, launchAff_, runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Exception (Error)

import Test.Spec (Spec, describe, it, pending', it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Parsing.Parser as P
import Parsing.Parser.String as P
import Parsing.Parser.Token as P
import Parsing.Parser.Combinators (choice, sepBy) as P

import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile, appendTextFile)

import Noodle.Text.QuickDef as QD
import Noodle.Text.QuickDefParser as QDP
import Noodle.Text.Generators as QTGen


in_file_paths = [ "./hydra.toolkit", "./hydra.v2.toolkit" ]
out_file_path i = "./test/" <> i <> ".tpurs"
out_file_path_sample i = "./test/" <> i <> ".tpurs.sample"
-- out_file_path_sample = "./test/hydra.toolkit.prepurs.sample"


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


spec :: Spec Unit
spec = do

  describe "Parse Toolkit definition from plain text description / Generate Toolkit code" $ do

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

    it "parsing one function with no arguments works, p.2" $ do
      parses
        "test : foo :: => Result {Empty}"
        (QD.qfm'' "test" "foo" [] "Result" "Empty")
        QDP.fnParser
      parses
        "test : foo :: <> => Result {Empty}"
        (QD.qfm'' "test" "foo" [] "Result" "Empty")
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

    it "parsing one function with one parametrized output works" $ do
      parses
        "source : noise :: <scale:Value {Number 10.0} -> offset:Value {Number 0.1}> => Texture {Empty}"
        (QD.qfmo "source" "noise"
          [ QD.qchantd "scale" "Value" "Number 10.0"
          , QD.qchantd "offset" "Value" "Number 0.1"
          ]
          [ QD.qchantd "out" "Texture" "Empty"
          ]
        )
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

    it "can parse several functions" $ do
      for_
        [ "synth : update :: <fn:UpdateFn> => Unit" /\ QD.qfm "synth" "update" [ QD.qchant "fn" "UpdateFn" ] "Unit"
        , "synth : render :: <from:From {All}> => Unit" /\ QD.qfm "synth" "render" [ QD.qchantd "from" "From" "All" ] "Unit"
        ]
        \(line /\ expectation) ->
          parses line expectation QDP.fnParser

    for_ in_file_paths \textFile -> do

      describe ("file " <> textFile) $ do
        -- file <- readTextFile UTF8 textFile
        -- let parseResult = P.runParser file QDP.familyListParser

        it "parses file" $ do
            fileContent <- readTextFile UTF8 textFile
            let parseResult = QDP.familyList fileContent
            case parseResult of
              Right result ->
                Array.length result `shouldEqual` 84
              Left error ->
                fail $ show error

        it "converts properly" $ do
            fileContent <- readTextFile UTF8 textFile
            let parseResult = QDP.familyList fileContent
            sampleContent <- readTextFile UTF8 $ out_file_path_sample textFile
            case parseResult of
              Right familiesList -> do

                let toolkit = QTGen.ToolkitName "hydra"
                let localsPrefix = QTGen.LocalsPrefix ""

                let toolkitSumType = QTGen.toolkitSumType toolkit familiesList
                let toolkitDataModule = QTGen.toolkitDataModule toolkit familiesList
                let toolkitModule = QTGen.toolkitModule QTGen.FamiliesAsModules toolkit localsPrefix [ "ModuleImport as MI" ] familiesList
                let toolkitModule' = QTGen.toolkitModule QTGen.FamiliesInline  toolkit localsPrefix [ "ModuleImport as MI" ] familiesList

                let familyModules = String.joinWith "\n\n{- MODULE -}\n\n" (QTGen.familyModule toolkit localsPrefix [ "FamilyImport as FI" ] <$> familiesList)

                let tpursFileContent =
                        String.joinWith "\n\n\n"
                          [ toolkitSumType
                          , toolkitDataModule
                          , toolkitModule
                          , toolkitModule'

                          , familyModules
                          ]
                writeTextFile UTF8 (out_file_path textFile) tpursFileContent
                _ <- Array.zipWithA shouldEqual
                          (String.split (String.Pattern "\n") tpursFileContent)
                          (String.split (String.Pattern "\n") sampleContent)
                pure unit
                -- fileContent `shouldEqual` sampleContent
              Left error ->
                fail $ show error



main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec