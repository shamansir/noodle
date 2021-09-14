module Test.Hydra.Compilation where

import Prelude (($), (#), Unit, discard)

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)
import Effect.Exception (Error, error)

import Data.Layout.Flex as O

import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Data.Vec2 ((<+>))


--import Hydra as H
import Hydra.API
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
            (osc (n 60.0) (n 0.1) (n 0.0)
                # out
            )
                `shouldCompileTo`
                "osc(60.0,0.1,0.0).out()"

        it "oscillator built with expressions (book example #1)" $ do

            -- Original: osc(Math.PI*2*10,0).out(o0)

            (osc2 (pi /*/ n 2.0 /*/ n 10.0) (n 0.0)
                # out' o0
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

            (queue
                [ osc2 (n 40.0) (n 0.0)
                    # out'' o0
                , src o0
                    # thresh0
                    # out'' o1
                , src o0
                    # posterize (n 3.0) (n 1.0)
                    # out'' o2
                , src o0
                    # pixelate (n 20.0) (n 20.0)
                    # out'' o3
                ]
            )
                `shouldCompileWithRenderTo`
                """osc(40.0,0.0,0.0).out(o0)
src(o0).thresh(0.5,0.04).out(o1)
src(o0).posterize(3.0,1.0).out(o2)
src(o0).pixelate(20.0,20.0).out(o3)
render()"""

        it "(book example #3)" $ do

            -- Original: osc(200, 0).kaleid(99).out(o0)

            (osc2 (n 200.0) (n 0.0)
                # kaleid (n 99.0)
                # out' o0
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

            (osc2 (n 40.0) (n 0.0)
                # thresh0
                # kaleid (n 99.0)
                # scale3 (n 1.0) (n 1.0) (dyn $ windowWidth /// windowHeight)
                # out' o0
            )
                `shouldCompileTo`
                "osc(40.0,0.0,0.0).thresh(0.5,0.04).kaleid(99.0).scale(1.0,1.0,() => (window.innerWidth/window.innerHeight),0.0,0.0).out(o0)"


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

            (shape2 (n 4.0) (n 0.9)
                # diff
                    (src o0
                        # scale1 (n 0.9)
                        # mask0 (shape (n 4.0) (n 0.9) (n 0.01))
                        # rotate1 (n 0.1)
                    )
                # out' o0
            )
                `shouldCompileTo`
                "shape(4.0,0.9,0.01).diff(src(o0).scale(0.9,1.0,1.0,0.0,0.0).mask(shape(4.0,0.9,0.01),3.0,0.5).rotate(0.1,0.0)).out(o0)"

        it "reusing pieces (book example #5)" $

            {- Original:

            n = 50;
            func = () => osc(30,0.1,1).modulate(noise(4,0.1))
            pix = () => shape(4,0.3).scale(1,1,3).repeat(n,n)
            pix().mult(func().color(1,0,0).pixelate(n,n)).out(o1)
            pix().mult(func().color(0,1,0).pixelate(n,n)).scrollX(1/n/3).out(o2)
            pix().mult(func().color(0,0,1).pixelate(n,n)).scrollX(2/n/3).out(o3)

            solid().add(src(o1),1).add(src(o2),1).add(src(o3),1).out(o0)
            -}
            let
                fifty = n 50.0
                func = osc (n 30.0) (n 0.1) (n 1.0) # modulate0 (noise (n 4.0) (n 0.1))
                pix = shape2 (n 4.0) (n 0.3) # scale3 (n 1.0) (n 1.0) (n 3.0) # repeat2 fifty fifty
            in do
                (queue
                    [ pix
                        # mult0 (func # color3 (n 1.0) (n 0.0) (n 0.0) # pixelate fifty fifty)
                        # out'' o1
                    , pix
                        # mult0 (func # color3 (n 0.0) (n 1.0) (n 0.0) # pixelate fifty fifty)
                        # scrollX1 (n 1.0 /// fifty /// n 3.0)
                        # out'' o2
                    , pix
                        # mult0 (func # color3 (n 0.0) (n 0.0) (n 1.0) # pixelate fifty fifty)
                        # scrollX1 (n 2.0 /// fifty /// n 3.0)
                        # out'' o3
                    , solid0
                        # add (src o1) (n 1.0)
                        # add (src o2) (n 1.0)
                        # add (src o3) (n 1.0)
                        # out'' o0
                    ]
                )
                    `shouldCompileTo`
                    """solid(0.0,0.0,0.0,1.0).add(src(o1),1.0).add(src(o2),1.0).add(src(o3),1.0).out(o0)
shape(4.0,0.3,0.01).scale(1.0,1.0,3.0,0.0,0.0).repeat(50.0,50.0,0.0,0.0).mult(osc(30.0,0.1,1.0).modulate(noise(4.0,0.1),0.1).color(1.0,0.0,0.0,1.0).pixelate(50.0,50.0),0.5).out(o1)
shape(4.0,0.3,0.01).scale(1.0,1.0,3.0,0.0,0.0).repeat(50.0,50.0,0.0,0.0).mult(osc(30.0,0.1,1.0).modulate(noise(4.0,0.1),0.1).color(0.0,1.0,0.0,1.0).pixelate(50.0,50.0),0.5).scrollX(((1.0/50.0)/3.0),0.0).out(o2)
shape(4.0,0.3,0.01).scale(1.0,1.0,3.0,0.0,0.0).repeat(50.0,50.0,0.0,0.0).mult(osc(30.0,0.1,1.0).modulate(noise(4.0,0.1),0.1).color(0.0,0.0,1.0,1.0).pixelate(50.0,50.0),0.5).scrollX(((2.0/50.0)/3.0),0.0).out(o3)"""


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