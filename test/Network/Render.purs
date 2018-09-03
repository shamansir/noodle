module RpdTest.Network.Render
    ( spec ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Rpd (init, Rpd, run, Network) as R
import Rpd.Render (Renderer(..), render)
import Rpd.Render.Terminal (TerminalRenderer, terminalRenderer)
import Rpd.Log as RL
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

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
      result <- liftEffect $ render myRenderer myRpd
      result `shouldEqual` "SUCC"
      pure unit
