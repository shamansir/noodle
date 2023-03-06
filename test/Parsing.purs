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
  , args :: Array (Maybe { name :: String, type :: String, default :: Maybe String })
  , returns :: String
  }


qfn :: String -> String -> Array { name :: String, type :: String, default :: Maybe String } -> String -> FN
qfn family name args = qfn' family name $ Just <$> args

qfn' :: String -> String -> Array (Maybe { name :: String, type :: String, default :: Maybe String }) -> String -> FN
qfn' family name args returns = FN { family, name, args, returns }


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
    validArgument = do
      P.choice
        [ P.char '?' *> pure Nothing
        ]
      -- _ <- validToken
      -- pure $ Just { name : "", type : "", default : Nothing }
    arrowSep = do
      _ <- Array.many P.space
      _ <- P.choice [ P.string "->", String.singleton <$> P.char '→' ]
      _ <- Array.many P.space
      pure unit
    argumentsP =
      P.between
        (P.char '(')
        (P.char ')')
        $ Array.fromFoldable <$> P.sepBy validArgument arrowSep


-- instance Show FN where
--   show (FN { family, name, args, returns }) = ""


derive newtype instance Show FN
derive newtype instance Eq FN


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Toolkit Defs to code" $ do
    it "parsing works" $
        -- file <- readTextFile UTF8 "./hydra.fn.clean.list"
        -- source : shape :: sides:Value (60) -> radius:Value (0.3) -> smoothing:Value (0.01) -> Result
        -- let parseResult = P.runParser "x" $ P.char 'x'
      parses "x" 'x' $ P.char 'x'

    it "parsing with sep works" $ do
        -- file <- readTextFile UTF8 "./hydra.fn.clean.list"
        -- source : shape :: sides:Value (60) -> radius:Value (0.3) -> smoothing:Value (0.01) -> Result
        -- let parseResult = P.runParser "x" $ P.char 'x'
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

    it "parsing one function with no arguments works" $
      parses
        "test : foo :: => Result"
        (qfn "test" "foo" [] "Result")
        myParser

    it "parsing one function with an unkwown argument works" $
      parses
        "test : foo :: (?) => Result"
        (qfn' "test" "foo" [ Nothing ] "Result")
        myParser

    it "parsing one function with two unkwown arguments works" $ do
      parses
        "test : foo :: (?->?) => Result"
        (qfn' "test" "foo" [ Nothing, Nothing ] "Result")
        myParser
      parses
        "test : foo :: (? -> ?) => Result"
        (qfn' "test" "foo" [ Nothing, Nothing ] "Result")
        myParser
      parses
        "test : foo :: (? ->?) => Result"
        (qfn' "test" "foo" [ Nothing, Nothing ] "Result")
        myParser
