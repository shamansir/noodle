module Test.Spec.Util.Parsing where

import Prelude

import Effect.Exception (Error)
import Effect.Class (class MonadEffect)

import Control.Monad.Error.Class (class MonadThrow)

import Data.Either (Either(..))
import Data.Identity (Identity)

import Parsing as P

import Test.Spec.Assertions (fail)

import Control.Monad.Error.Class (class MonadThrow)

import Data.FoldableWithIndex (foldlWithIndex, class FoldableWithIndex)
import Data.Text.Diff (class Diffable, toDiffString)
import Data.Text.Diff (compareBy, Comparator(..)) as Diff

import Test.Spec (SpecT, it, class Example)
import Test.Spec.Util.Assertions (shouldEqual) as U



parses' :: forall (s :: Type) (m :: Type -> Type) (a :: Type). Diffable a => MonadEffect m => MonadThrow Error m => Eq a => s -> a -> P.ParserT s Identity a -> m Unit
parses' =
  parses identity


parses :: forall (x :: Type) (s :: Type) (m :: Type -> Type) (a :: Type). MonadEffect m => MonadThrow Error m => Eq x => Diffable x => (a -> x) -> s -> a -> P.ParserT s Identity a -> m Unit
parses atostr string expected parser =
  case P.runParser string parser of
    Right result -> do
      (toDiffString (atostr result)) `U.shouldEqual` (toDiffString (atostr expected))
    Left error ->
      fail $ show error