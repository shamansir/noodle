module RpdTest.Network.Render
    ( spec ) where

import Prelude

import Effect.Class (liftEffect)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Rpd (init) as R
import Rpd.API (Rpd) as R
import Rpd.Network (Network) as R
import Rpd.Render (once) as Render
import Rpd.Render.Terminal (TerminalRenderer, terminalRenderer)


data MyData
  = Bang
  | Value Int

type MyRpd = R.Rpd (R.Network MyData)

type MyRenderer = TerminalRenderer MyData


myRenderer :: MyRenderer
myRenderer = terminalRenderer


myRpd :: MyRpd
myRpd =
  R.init "f"


spec :: Spec Unit
spec =
  describe "rendering" do
    it "rendering the network works" do
      result <- liftEffect $ Render.once myRenderer myRpd
      result `shouldEqual` "SUCC"
      pure unit
