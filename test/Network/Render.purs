module RpdTest.Network.Render
    ( spec ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Aff (Aff())

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Rpd (init) as R
import Rpd.API as R
import Rpd.API ((</>))
import Rpd.Path
import Rpd.Network (Network) as R
import Rpd.Render (once, Renderer) as Render
import Rpd.RenderMUV (once, Renderer) as RenderMUV
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
      expectToRenderOnce stringRenderer myRpd
        "Network foo:\nNo Patches\nNo Links\n"
      expectToRenderOnceMUV terminalRenderer myRpd "{>}"
      pure unit
    it "rendering the single node works" do
      let
        singleNodeNW = myRpd
          </> R.addPatch "foo"
          </> R.addNode (patchId 0) "bar"
      expectToRenderOnce stringRenderer singleNodeNW $
        "Network foo:\nOne Patch\nPatch foo P0:\n" <>
          "One Node\nNode bar P0/N0:\n" <>
            "No Inlets\nNo Outlets\nNo Links\n"
      expectToRenderOnceMUV terminalRenderer singleNodeNW "..."
      pure unit
    it "rendering the erroneous network responds with the error" do
      let
        erroneousNW = myRpd
          -- add inlet to non-exising node
          </> R.addInlet (nodePath 0 0) "foo"
      expectToRenderOnce stringRenderer erroneousNW "<>"
      expectToRenderOnceMUV terminalRenderer erroneousNW "ERR: "
      pure unit


expectToRenderOnce
  :: forall d
   . Render.Renderer d String
  -> R.Rpd (R.Network d)
  -> String
  -> Aff Unit
expectToRenderOnce renderer rpd expectation = do
  result <- liftEffect $ Render.once renderer rpd
  result `shouldEqual` expectation


expectToRenderOnceMUV
  :: forall d x
   . RenderMUV.Renderer d x String
  -> R.Rpd (R.Network d)
  -> String
  -> Aff Unit
expectToRenderOnceMUV renderer rpd expectation = do
  result <- liftEffect $ RenderMUV.once renderer rpd
  result `shouldEqual` expectation
