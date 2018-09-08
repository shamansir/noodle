module RpdTest.Network.Render
    ( spec ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Aff (Aff())

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Rpd (init) as R
import Rpd.API (Rpd) as R
import Rpd.Network (Network) as R
import Rpd.Render (once, Renderer) as Render
import Rpd.RenderS (once, Renderer) as RenderS
import Rpd.Renderer.Terminal (terminalRenderer)
import Rpd.Renderer.String (stringRenderer)


data MyData
  = Bang
  | Value Int

type MyRpd = R.Rpd (R.Network MyData)


myRpd :: MyRpd
myRpd =
  R.init "foo"


spec :: Spec Unit
spec =
  describe "rendering" do
    it "rendering the empty network works" do
      expectToRenderOnceS terminalRenderer myRpd "SUCC"
      expectToRenderOnce stringRenderer myRpd
        "Network foo:\nNo Patches\nNo Links\n"
      pure unit


expectToRenderOnceS
  :: forall d x
   . RenderS.Renderer d x String
  -> R.Rpd (R.Network d)
  -> String
  -> Aff Unit
expectToRenderOnceS renderer rpd expectation = do
  result <- liftEffect $ RenderS.once renderer rpd
  result `shouldEqual` expectation

expectToRenderOnce
  :: forall d
   . Render.Renderer d String
  -> R.Rpd (R.Network d)
  -> String
  -> Aff Unit
expectToRenderOnce renderer rpd expectation = do
  result <- liftEffect $ Render.once renderer rpd
  result `shouldEqual` expectation
