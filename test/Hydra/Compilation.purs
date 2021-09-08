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

        it "Book example #2" $ do
            (Queue.fromFoldable
                [ H.O0 /\
                    (H.textureOf
                        $ H.Osc
                            { freq : H.Num 40.0, sync : H.Num 0.0, offset : H.Num 0.0 }
                    )
                , H.O1 /\
                    H.withModifiers
                        (H.textureOf $ H.Source H.O0)
                        [ H.color $ H.Tresh { treshold : H.Num 0.5, tolerance : H.Num 0.04 } ]
                , H.O2 /\
                    H.withModifiers
                        (H.textureOf $ H.Source H.O0)
                        [ H.color $ H.Posterize { bins : H.Num 3.0, gamma : H.Num 1.0 } ]
                , H.O3 /\
                    H.withModifiers
                        (H.textureOf $ H.Source H.O0)
                        [ H.geometry $ H.Pixelate { pixelX : H.Num 20.0, pixelY : H.Num 20.0 } ]
                ]
            )
                `shouldCompileWithRenderTo`
                """osc(40.0,0.0,0.0).out(o0)
src(o0).tresh(0.5,0.04).out(o1)
src(o0).posterize(3.0,1.0).out(o2)
src(o0).pixelate(20.0,20.0).out(o3)
render()"""

        it "Book example #3" $ do
            (Queue.atBuffer H.O0
                $ H.withModifiers
                    (H.textureOf
                        $ H.Osc
                            { freq : H.Num 200.0, sync : H.Num 0.0, offset : H.Num 0.0 }
                    )
                [ H.geometry $ H.Kaleid { nSides : H.Num 99.0 } ]

              )
                `shouldCompileTo`
                "osc(200.0,0.0,0.0).kaleid(99.0).out(o0)"

        it "Book example #3, v2" $ do
            (Queue.atBuffer H.O0
                $ H.withModifiers
                    (H.textureOf
                        $ H.Osc
                            { freq : H.Num 40.0, sync : H.Num 0.0, offset : H.Num 0.0 }
                    )
                [ H.color $ H.Tresh { treshold : H.Num 0.5, tolerance : H.Num 0.04 }
                , H.geometry $ H.Kaleid { nSides : H.Num 99.0 }
                , H.geometry $ H.Scale
                    { amount : H.Num 1.0
                    , xMult : H.Num 1.0
                    , yMult : H.Dynamic $ H.Expr H.WindowWidth H.Divide H.WindowHeight
                    , offsetX : H.Num 0.0
                    , offsetY : H.Num 0.0
                    }
                ]

              )
                `shouldCompileTo`
                "osc(40.0,0.0,0.0).tresh(0.5,0.04).kaleid(99.0).scale(1.0,1.0,() => (window.innerWidth/window.innerHeight),0.0,0.0).out(o0)"


shouldCompileTo
  :: forall m
   . MonadThrow Error m
  => Queue
  -> String
  -> m Unit
shouldCompileTo queue str =
  compile Compiler.compact queue `shouldEqual` str


shouldCompileWithRenderTo
  :: forall m
   . MonadThrow Error m
  => Queue
  -> String
  -> m Unit
shouldCompileWithRenderTo queue str =
  compileWithRender Compiler.compact queue `shouldEqual` str