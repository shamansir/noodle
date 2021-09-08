module Test.Hydra.Compilation where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)
import Effect.Exception (Error, error)

import Data.Layout.Flex as O

import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Data.Vec2 ((<+>))


import Hydra as H
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

        it "just oscillator" $ do
            (Queue.just
                $ H.textureOf
                $ H.Osc { freq : H.Num 60.0, sync : H.Num 0.1, offset : H.Num 0.0 }
            )
                `shouldCompileTo`
                "osc(60.0,0.1,0.0).out()"

        it "Book example #1" $ do
            (Queue.atBuffer H.O0
                $ H.textureOf
                $ H.Osc
                    { freq : H.Expr (H.Expr H.Pi H.Multiply $ H.Num 2.0) H.Multiply $ H.Num 10.0
                    , sync : H.Num 0.0
                    , offset : H.Num 0.0
                    }
              )
                `shouldCompileTo`
                "osc(((Math.PI*2.0)*10.0),0.0,0.0).out(o0)"


shouldCompileTo
  :: forall m
   . MonadThrow Error m
  => Queue
  -> String
  -> m Unit
shouldCompileTo queue str =
  compile Compiler.compact queue `shouldEqual` str