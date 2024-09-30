module Test.Spec.Util.Parsing where

import Prelude

import Effect.Exception (Error)

import Control.Monad.Error.Class (class MonadThrow)

import Data.Either (Either(..))
import Data.Identity (Identity)

import Parsing as P

import Test.Spec.Assertions (fail)
import Test.Spec.Util.Assertions (shouldEqual, class PrettyPrint) as U


parses' :: forall (s :: Type) (m :: Type -> Type) (a :: Type). MonadThrow Error m => Eq a => U.PrettyPrint a => s -> a -> P.ParserT s Identity a -> m Unit
parses' =
  parses identity


parses :: forall (x :: Type) (s :: Type) (m :: Type -> Type) (a :: Type). MonadThrow Error m => Eq x => U.PrettyPrint x => (a -> x) -> s -> a -> P.ParserT s Identity a -> m Unit
parses atostr string expected parser =
  case P.runParser string parser of
    Right result ->
      (atostr result) `U.shouldEqual` (atostr expected)
    Left error ->
      fail $ show error