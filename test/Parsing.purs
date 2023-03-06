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
import Text.Parsing.Parser.Combinators as P

import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)


{-
loadFile :: forall m. MonadEffect m => m Unit
loadFile =
    liftEffect
        $ runAff_ (\_ -> pure unit)
        $ readTextFile UTF8 "./hydra.fn.clean.list"
-}


newtype FN = FN
  { family :: String
  , name :: String
  , args :: Array (Maybe Argument)
  , returns :: String
  }

type Argument =
  { name :: String, type :: Maybe String, default :: Maybe String }


qfn :: String -> String -> Array Argument -> String -> FN
qfn family name args = qfn' family name $ Just <$> args

qfn' :: String -> String -> Array (Maybe Argument) -> String -> FN
qfn' family name args returns = FN { family, name, args, returns }

qarg :: String -> Argument
qarg name = { name, type : Nothing, default : Nothing }

qarg' :: String -> Maybe Argument
qarg' = qarg >>> Just

qargt :: String -> String -> Argument
qargt name t = { name, type : Just t, default : Nothing }

qargt' :: String -> String -> Maybe Argument
qargt' n = qargt n >>> Just

qargtd :: String -> String -> String -> Argument
qargtd name t d = { name, type : Just t, default : Just d }

qargtd' :: String -> String -> String -> Maybe Argument
qargtd' n t = qargtd n t >>> Just


parses :: forall (s :: Type) (m :: Type -> Type) (a :: Type). MonadThrow Error m => Show a => Eq a => s -> a -> P.ParserT s Identity a -> m Unit
parses string expected parser =
  case P.runParser string parser of
    Right result ->
      result `shouldEqual` expected
    Left error ->
      fail $ show error


myParser :: forall (m :: Type -> Type). Functor m => Monad m => P.ParserT String m FN
myParser = do
  family <- validToken
  _ <- Array.some P.space
  _ <- P.char ':'
  _ <- Array.some P.space
  fnName <- validToken
  _ <- Array.some P.space
  _ <- P.string "::"
  _ <- Array.many P.space
  args <- P.option [] argumentsP
  _ <- Array.many P.space
  _ <- P.string "=>"
  _ <- Array.some P.space
  returnVal <- validToken
  pure $ qfn' family fnName args returnVal

  where
    validToken = String.fromCharArray <$> Array.some P.alphaNum
    validDefaultValue = String.fromCharArray <$> Array.some (P.choice [ P.alphaNum, P.char '.' ])
    validArgument = do
      P.choice
        [ P.char '?' *> pure Nothing
        , Just <$> do
            name <- validToken
            maybeType <- P.optionMaybe $ P.char ':' *> validToken
            _ <- Array.many P.space
            maybeDefault <- P.optionMaybe
                              $ P.between
                                (P.char '{')
                                (P.char '}')
                                validDefaultValue
            pure { name, type : maybeType, default : maybeDefault }
        ]
    arrowSep = do
      _ <- Array.many P.space
      _ <- P.choice [ P.string "->", String.singleton <$> P.char '→' ]
      _ <- Array.many P.space
      pure unit
    argumentsP =
      P.between
        (P.char '<')
        (P.char '>')
        $ Array.fromFoldable <$> P.sepBy validArgument arrowSep


-- instance Show FN where
--   show (FN { family, name, args, returns }) = ""


derive newtype instance Show FN
derive newtype instance Eq FN


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
        (qfn "test" "foo" [] "Result")
        myParser
      parses
        "test : foo :: <> => Result"
        (qfn "test" "foo" [] "Result")
        myParser

    it "parsing one function with an unkwown argument works" $
      parses
        "test : foo :: <?> => Result"
        (qfn' "test" "foo" [ Nothing ] "Result")
        myParser

    it "parsing one function with two unkwown arguments works" $ do
      parses
        "test : foo :: <?->?> => Result"
        (qfn' "test" "foo" [ Nothing, Nothing ] "Result")
        myParser
      parses
        "test : foo :: <? -> ?> => Result"
        (qfn' "test" "foo" [ Nothing, Nothing ] "Result")
        myParser
      parses
        "test : foo :: <? ->?> => Result"
        (qfn' "test" "foo" [ Nothing, Nothing ] "Result")
        myParser

    it "parsing one function with arguments works" $ do
      parses
        "test : foo :: <bar->?> => Result"
        (qfn' "test" "foo" [ Just (qarg "bar"), Nothing ] "Result")
        myParser
      parses
        "test : foo :: <? -> bar> => Result"
        (qfn' "test" "foo" [ Nothing, Just (qarg "bar") ] "Result")
        myParser
      parses
        "test : foo :: <bar:Number -> buz:Number {20}> => Result"
        (qfn "test" "foo" [ qargt "bar" "Number", qargtd "buz" "Number" "20" ] "Result")
        myParser
      parses
        "modulate : modulateRepeat :: <what:Texture -> with:Texture -> repeatX:Value {3} -> repeatY:Value {3} -> offsetX:Value {0.5} -> offsetY:Value {0.5}> => Texture"
        (qfn "modulate" "modulateRepeat"
          [ qargt "what" "Texture"
          , qargt "with" "Texture"
          , qargtd "repeatX" "Value" "3"
          , qargtd "repeatY" "Value" "3"
          , qargtd "offsetX" "Value" "0.5"
          , qargtd "offsetY" "Value" "0.5"
          ]
        "Texture")
        myParser
      parses
        "synth : render :: <from:From {All}> => Unit"
        (qfn "synth" "render"
          [ qargtd "from" "From" "All"
          ]
        "Unit")
        myParser
      parses
        "synth : update :: <fn:UpdateFn> => Unit"
        (qfn "synth" "update"
          [ qargt "fn" "UpdateFn"
          ]
        "Unit")
        myParser


    it "parses file" $ do
        file <- readTextFile UTF8 "./hydra.fn.clean.list"
        let parseResult = P.runParser file $ Array.many $ myParser >>= \fn -> P.char '\n' *> pure fn
        case parseResult of
          Right result ->
            Array.length result `shouldEqual` 84
          Left error ->
            fail $ show error