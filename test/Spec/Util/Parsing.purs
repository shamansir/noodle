module Test.Spec.Util.Parsing where

import Prelude

import Effect.Exception (Error)

import Control.Monad.Error.Class (class MonadThrow)

import Data.Either (Either(..))
import Data.Identity (Identity)

import Parsing as P

import Test.Spec.Assertions (shouldEqual, fail)


parses' :: forall (s :: Type) (m :: Type -> Type) (a :: Type). MonadThrow Error m => Show a => Eq a => s -> a -> P.ParserT s Identity a -> m Unit
parses' =
  parses identity


parses :: forall (x :: Type) (s :: Type) (m :: Type -> Type) (a :: Type). MonadThrow Error m => Eq x => Show x => (a -> x) -> s -> a -> P.ParserT s Identity a -> m Unit
parses atostr string expected parser =
  case P.runParser string parser of
    Right result ->
      (atostr result) `shouldEqual` (atostr expected)
    Left error ->
      fail $ show error