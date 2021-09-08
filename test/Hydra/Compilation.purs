module Test.Hydra.Compilation where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)
import Effect.Exception (Error, error)

import Data.Layout.Flex as O

import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Data.Vec2 ((<+>))


import Hydra.Compile (compact) as Compiler
import Hydra.Compile (compile, compileWithRender)
import Hydra.Queue (Queue)
import Hydra.Queue as Queue

import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)


spec :: Spec Unit
spec = do
    describe "Compilation" do

        it "empty queue" $ do
            Queue.empty `shouldCompileTo` ""


shouldCompileTo
  :: forall m
   . MonadThrow Error m
  => Queue
  -> String
  -> m Unit
shouldCompileTo queue str =
  compile Compiler.compact queue `shouldEqual` str