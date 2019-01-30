module RpdTest.Flow.Links
    ( spec
    ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))

import FRP.Event.Time (interval)

import Rpd.API ((</>))
import Rpd.API as R
import Rpd.Path
import Rpd.Util (flow) as R

import Test.Spec (Spec, it, pending)
import Test.Spec.Assertions (shouldContain, shouldNotContain)

import RpdTest.Util (withRpd)
import RpdTest.CollectData (TraceItem(..))
import RpdTest.CollectData as CollectData
import RpdTest.Flow.Base (MyRpd, Delivery(..))




{- ======================================= -}
{- ================ LINKS ================ -}
{- ======================================= -}


spec :: Spec Unit
spec = do
  it "connecting some outlet to some inlet makes data flow from this outlet to this inlet" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (patchId 0) "node1"
          </> R.addOutlet (nodePath 0 0) "outlet"
          </> R.addNode (patchId 0) "node2"
          </> R.addInlet (nodePath 0 1) "inlet"

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            cancel <-
              nw # R.connect (outletPath 0 0 0) (inletPath 0 1 0)
                  </> R.streamToOutlet
                      (outletPath 0 0 0)
                      (R.flow $ const Notebook <$> interval 30)
            pure [ cancel ]
        collectedData `shouldContain`
          (OutletData (outletPath 0 0 0) Notebook)
        collectedData `shouldContain`
          (InletData (inletPath 0 1 0) Notebook)
        pure unit

    pure unit

  it "connecting some outlet having its own flow to some inlet directs this existing flow to this inlet" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (patchId 0) "node1"
          </> R.addOutlet (nodePath 0 0) "outlet"
          </> R.addNode (patchId 0) "node2"
          </> R.addInlet (nodePath 0 1) "inlet"

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            cancel <-
              nw # R.streamToOutlet
                (outletPath 0 0 0)
                (R.flow $ const Notebook <$> interval 30)
            _ <- nw # R.connect
                (outletPath 0 0 0)
                (inletPath 0 1 0)
            pure [ cancel ]
        collectedData `shouldContain`
          (OutletData (outletPath 0 0 0) Notebook)
        collectedData `shouldContain`
          (InletData (inletPath 0 1 0) Notebook)
        pure unit

    pure unit

  it "disconnecting some outlet from some inlet makes the data flow between them stop" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (patchId 0) "node1"
          </> R.addOutlet (nodePath 0 0) "outlet"
          </> R.addNode (patchId 0) "node2"
          </> R.addInlet (nodePath 0 1) "inlet"

    rpd # withRpd \nw -> do
        nw' /\ collectedData <- CollectData.channelsAfter'
          (Milliseconds 100.0)
          nw
          $ do
            -- NB:we're not cancelling this data flow between checks
            _ <- nw # R.streamToOutlet
                  (outletPath 0 0 0)
                  (R.flow $ const Notebook <$> interval 30)
            nw' <- nw # R.connect
                  (outletPath 0 0 0)
                  (inletPath 0 1 0)
            pure $ nw' /\ []
        collectedData `shouldContain`
          (InletData (inletPath 0 1 0) Notebook)
        collectedData' <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw'
          $ do
            _ <-
              nw' # R.disconnectAll (outletPath 0 0 0) (inletPath 0 1 0)
            pure [ ]
        collectedData' `shouldContain`
          (OutletData (outletPath 0 0 0) Notebook)
        collectedData' `shouldNotContain`
          (InletData (inletPath 0 1 0) Notebook)
        pure unit


  pending "default value of the inlet is sent on connection"

  pending "default value for the inlet is sent on disconnection"

  pending "looped connections should be disallowed"
    -- (to the same node, etc.)

    -- describe "processing the output from nodes" do
    --   describe "with predefined function" do
    --     pure unit
    --   describe "with function defined after creation" do
    --     pure unit
    --   describe "after adding an inlet" do
    --     pure unit
    --   describe "after removing an inlet" do
    --     pure unit
    --   describe "after adding an outlet" do
    --     pure unit
    --   describe "after removing an outlet" do
    --     pure unit
    --   describe "after changing the node structure" do
    --     pure unit
    --   describe "after deleting the receiving node" do
    --     pure unit
    --   describe "after adding new node" do
    --     pure unit
