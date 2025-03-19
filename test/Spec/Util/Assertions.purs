module Test.Spec.Util.Assertions where

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
import Data.Text.Diff (compareBy, Comparator(..), Limit(..)) as Diff

import Test.Spec (SpecT, it, class Example)



shouldEqual :: forall m. MonadEffect m => MonadThrow Error m => String -> String -> m Unit
shouldEqual = Diff.compareBy $ Diff.OnlyDifferent $ Diff.Limit 10


shouldEqualStack :: forall m. MonadEffect m => MonadThrow Error m => String -> String -> m Unit
shouldEqualStack = Diff.compareBy $ Diff.Stack Diff.NoLimit