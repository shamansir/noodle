module Test.Hydra.Compilation where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)
import Effect.Exception (Error, error)

import Data.Layout.Flex as O

import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Data.Vec2 ((<+>))


--import Hydra as H
import Hydra.API as H
import Hydra.API ((/*/), (///))
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
            (H.osc (H.n 60.0) (H.n 0.1) (H.n 0.0)
                # H.out
            )
                `shouldCompileTo`
                "osc(60.0,0.1,0.0).out()"

        it "oscillator built with expressions (book example #1)" $ do

            -- Original: osc(Math.PI*2*10,0).out(o0)

            (H.osc2 (H.pi /*/ H.n 2.0 /*/ H.n 10.0) (H.n 0.0)
                # H.out' H.o0
            )
                `shouldCompileTo`
                "osc(((Math.PI*2.0)*10.0),0.0,0.0).out(o0)"

        it "several buffers sent to `render` (book example #2)" $ do

            {- Original:

            osc(40,0).out(o0)
            src(o0).thresh().out(o1)
            src(o0).posterize(3,1).out(o2)
            src(o0).pixelate(20, 20).out(o3)
            render()

            -}

            (H.queue
                [ H.osc2 (H.n 40.0) (H.n 0.0)
                    # H.out'' H.o0
                , H.src H.o0
                    # H.tresh0
                    # H.out'' H.o1
                , H.src H.o0
                    # H.posterize (H.n 3.0) (H.n 1.0)
                    # H.out'' H.o2
                , H.src H.o0
                    # H.pixelate (H.n 20.0) (H.n 20.0)
                    # H.out'' H.o3
                ]
            )
                `shouldCompileWithRenderTo`
                """osc(40.0,0.0,0.0).out(o0)
src(o0).tresh(0.5,0.04).out(o1)
src(o0).posterize(3.0,1.0).out(o2)
src(o0).pixelate(20.0,20.0).out(o3)
render()"""

        it "(book example #3)" $ do

            -- Original: osc(200, 0).kaleid(99).out(o0)

            (H.osc2 (H.n 200.0) (H.n 0.0)
                # H.kaleid (H.n 99.0)
                # H.out' H.o0
            )
                `shouldCompileTo`
                "osc(200.0,0.0,0.0).kaleid(99.0).out(o0)"

        it "modifier with a function inside (book example #3.2)" $ do

            {- Original:

            osc(40,0)
                .thresh()
                .kaleid(3)
                .scale(1,1,()=>window.innerWidth/window.innerHeight)
                .out(o0)
            -}

            (H.osc2 (H.n 40.0) (H.n 0.0)
                # H.tresh0
                # H.kaleid (H.n 99.0)
                # H.scale3 (H.n 1.0) (H.n 1.0) (H.dyn $ H.windowWidth /// H.windowHeight)
                # H.out' H.o0
            )
                `shouldCompileTo`
                "osc(40.0,0.0,0.0).tresh(0.5,0.04).kaleid(99.0).scale(1.0,1.0,() => (window.innerWidth/window.innerHeight),0.0,0.0).out(o0)"


        it "blends, where texture is an argument (book example #10)" $ do

            {- Original:

            shape(4,0.9)
                .diff(
                    src(o0)
                        .scale(0.9)
                        .mask(shape(4,0.9,0.01))
                        .rotate(0.1)
                )
                .out(o0)
            -}

            (H.shape2 (H.n 4.0) (H.n 0.9)
                # H.diff
                    (H.src H.o0
                        # H.scale1 (H.n 0.9)
                        # H.mask0 (H.shape (H.n 4.0) (H.n 0.9) (H.n 0.01))
                        # H.rotate1 (H.n 0.1)
                    )
                # H.out' H.o0
            )
                `shouldCompileTo`
                "shape(4.0,0.9,0.01).diff(src(o0).scale(0.9,1.0,1.0,0.0,0.0).mask(shape(4.0,0.9,0.01),3.0,0.5).rotate(0.1,0.0)).out(o0)"


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