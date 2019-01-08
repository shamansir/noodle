module RpdTest.Network.Structure
    ( spec ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Class.Console (log)

import Test.Spec (Spec, describe, it, pending)

import Rpd (init, run') as R
import Rpd.API (Rpd) as R
import Rpd.Network (Network) as R


data MyData
  = Bang

type MyRpd = R.Rpd (R.Network MyData)

myRpd :: MyRpd
myRpd =
  R.init "f"

spec :: Spec Unit
spec =
  describe "structure" do
    it "constructing the empty network works" do
      -- FIXME: fail on error
      _ <- liftEffect $ R.run' (log <<< show) (const $ pure unit) myRpd
      pure unit
    pending "paths are not getting messed up when you add/remove things"
      -- test if removing and then adding new node/inlet/outlet/etc... keeps paths
      -- unique: e.g. the node has three inlets (P0/N0/I0, P0/N0/I1, P0/N0/I2),
      -- you remove the one in the middle (P0/N0/I0, P0/N0/I2) and add the new one
      -- on the top (also P0/N0/I2, since the size of inlets is two now)
