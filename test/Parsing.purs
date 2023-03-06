module Test.Parsing where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.String.CodeUnits as String
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
import Node.FS.Aff (readTextFile)

import Noodle.Text.QuickDef as QD
import Noodle.Text.QuickDefParser as QDP


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
        (QD.qfn "test" "foo" [] "Result")
        QDP.fnParser
      parses
        "test : foo :: <> => Result"
        (QD.qfn "test" "foo" [] "Result")
        QDP.fnParser

    it "parsing one function with an unkwown argument works" $
      parses
        "test : foo :: <?> => Result"
        (QD.qfn' "test" "foo" [ Nothing ] "Result")
        QDP.fnParser

    it "parsing one function with two unkwown arguments works" $ do
      parses
        "test : foo :: <?->?> => Result"
        (QD.qfn' "test" "foo" [ Nothing, Nothing ] "Result")
        QDP.fnParser
      parses
        "test : foo :: <? -> ?> => Result"
        (QD.qfn' "test" "foo" [ Nothing, Nothing ] "Result")
        QDP.fnParser
      parses
        "test : foo :: <? ->?> => Result"
        (QD.qfn' "test" "foo" [ Nothing, Nothing ] "Result")
        QDP.fnParser

    it "parsing one function with arguments works" $ do
      parses
        "test : foo :: <bar->?> => Result"
        (QD.qfn' "test" "foo" [ Just (QD.qarg "bar"), Nothing ] "Result")
        QDP.fnParser
      parses
        "test : foo :: <? -> bar> => Result"
        (QD.qfn' "test" "foo" [ Nothing, Just (QD.qarg "bar") ] "Result")
        QDP.fnParser
      parses
        "test : foo :: <bar:Number -> buz:Number {20}> => Result"
        (QD.qfn "test" "foo" [ QD.qargt "bar" "Number", QD.qargtd "buz" "Number" "20" ] "Result")
        QDP.fnParser
      parses
        "modulate : modulateRepeat :: <what:Texture -> with:Texture -> repeatX:Value {3} -> repeatY:Value {3} -> offsetX:Value {0.5} -> offsetY:Value {0.5}> => Texture"
        (QD.qfn "modulate" "modulateRepeat"
          [ QD.qargt "what" "Texture"
          , QD.qargt "with" "Texture"
          , QD.qargtd "repeatX" "Value" "3"
          , QD.qargtd "repeatY" "Value" "3"
          , QD.qargtd "offsetX" "Value" "0.5"
          , QD.qargtd "offsetY" "Value" "0.5"
          ]
        "Texture")
        QDP.fnParser
      parses
        "synth : render :: <from:From {All}> => Unit"
        (QD.qfn "synth" "render"
          [ QD.qargtd "from" "From" "All"
          ]
        "Unit")
        QDP.fnParser
      parses
        "synth : update :: <fn:UpdateFn> => Unit"
        (QD.qfn "synth" "update"
          [ QD.qargt "fn" "UpdateFn"
          ]
        "Unit")
        QDP.fnParser

    it "parses file" $ do
        file <- readTextFile UTF8 "./hydra.fn.clean.list"
        let parseResult = P.runParser file QDP.fnListParser
        case parseResult of
          Right result ->
            Array.length result `shouldEqual` 84
          Left error ->
            fail $ show error