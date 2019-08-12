module RpdTest.Flow.Links
    ( spec
    ) where

import Prelude

import Effect.Ref as Ref
import Effect.Aff (delay)

import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))

import FRP.Event.Time (interval)

import Rpd.API.Action.Sequence ((</>))
import Rpd.API.Action.Sequence (init) as Actions
import Rpd.API.Action.Sequence as R
import Rpd.Path
import Rpd.Util (flow) as R
import Rpd.Network (empty) as Network

import Test.Spec (Spec, it, pending)
import Test.Spec.Assertions (shouldContain, shouldNotContain)

-- import RpdTest.CollectData (TraceItem(..))
import RpdTest.Helper (channelsAfter) as CollectData
import RpdTest.Helper (TraceItem(..), withRpd', (+>))
import RpdTest.Flow.Base (Actions, Network, Delivery(..), Pipe(..), Node(..), myToolkit)


{- ======================================= -}
{- ================ LINKS ================ -}
{- ======================================= -}


spec :: Spec Unit
spec = do
  it "connecting some outlet to some inlet makes data flow from this outlet to this inlet" $ do
    let
      structure :: Actions
      structure =
        Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node1" Empty
          </> R.addOutlet (toNode "patch" "node1") "outlet" Pass
          </> R.addNode (toPatch "patch") "node2" Empty
          </> R.addInlet (toNode "patch" "node2") "inlet" Pass

    collectedData <- CollectData.channelsAfter
      (Milliseconds 100.0)
      myToolkit
      (Network.empty "network")
      $ structure
          -- first connect, then stream
          </> R.connect
              (toOutlet "patch" "node1" "outlet")
              (toInlet "patch" "node2" "inlet")
          </> R.streamToOutlet
              (toOutlet "patch" "node1" "outlet")
              (R.flow $ const Notebook <$> interval 30)
    collectedData `shouldContain`
      (OutletData (toOutlet "patch" "node1" "outlet") Notebook)
    collectedData `shouldContain`
      (InletData (toInlet "patch" "node2" "inlet") Notebook)
    pure unit

  it "connecting some outlet having its own flow to some inlet directs this existing flow to this inlet" $ do
    let
      structure :: Actions
      structure =
        Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node1" Empty
          </> R.addOutlet (toNode "patch" "node1") "outlet" Pass
          </> R.addNode (toPatch "patch") "node2" Empty
          </> R.addInlet (toNode "patch" "node2") "inlet" Pass

    collectedData <- CollectData.channelsAfter
      (Milliseconds 100.0)
      myToolkit
      (Network.empty "network")
      $ structure
              -- first stream, then connect
            </> R.streamToOutlet
                      (toOutlet "patch" "node1" "outlet")
                      (R.flow $ const Notebook <$> interval 30)
            </> R.connect
                      (toOutlet "patch" "node1" "outlet")
                      (toInlet "patch" "node2" "inlet")
    collectedData `shouldContain`
      (OutletData (toOutlet "patch" "node1" "outlet") Notebook)
    collectedData `shouldContain`
      (InletData (toInlet "patch" "node2" "inlet") Notebook)

    pure unit

  it "disconnecting some outlet from some inlet makes the data flow between them stop" $ do
    let
      structure :: Actions
      structure =
        Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node1" Empty
          </> R.addOutlet (toNode "patch" "node1") "outlet" Pass
          </> R.addNode (toPatch "patch") "node2" Empty
          </> R.addInlet (toNode "patch" "node2") "inlet" Pass

    -- collectedData <- CollectData.channelsAfter
    --   (Milliseconds 100.0)
    --   $ structure
    --           -- first stream, then connect
    --         </> R.streamToOutlet
    --                   (toOutlet "patch" "node1" "outlet")
    --                   (R.flow $ const Notebook <$> interval 30)
    --         </> R.connect
    --                   (toOutlet "patch" "node1" "outlet")
    --                   (toInlet "patch" "node2" "inlet")

    -- collectedData `shouldContain`
    --   (OutletData (toOutlet "patch" "node1" "outlet") Notebook)
    -- collectedData `shouldContain`
    --   (InletData (toInlet "patch" "node2" "inlet") Notebook)
    collectedData <- Ref.new []
    withRpd' myToolkit (Network.empty "network") $ structure
      </> R.streamToOutlet
                  (toOutlet "patch" "node1" "outlet")
                  (R.flow $ const Notebook <$> interval 30)
      </> R.connect
                  (toOutlet "patch" "node1" "outlet")
                  (toInlet "patch" "node2" "inlet")
      </> R.subscribeToInlet (toInlet "patch" "node2" "inlet")
            (\d -> do
              curData <- Ref.read collectedData
              Ref.write (d +> curData) collectedData)
      </> R.do_
            (\_ -> do
              delay (Milliseconds 100.0)
              collectedData `shouldContain` Notebook
              Ref.write [] collectedData
            )
      </> R.disconnect
                  (toOutlet "patch" "node1" "outlet")
                  (toInlet "patch" "node2" "inlet")
      </> R.do_
            (\_ -> do
              delay (Milliseconds 100.0)
              collectedData `shouldNotContain` Notebook
              Ref.write [] collectedData
            )


    -- rpd # withRpd \nw -> do
    --     nw' /\ collectedData <- CollectData.channelsAfter'
    --       (Milliseconds 100.0)
    --       nw
    --       $ do
    --         -- NB: ensure we're really cancelling this data flow between checks
    --         _ <- nw # R.streamToOutlet
    --               (toOutlet "patch" "node1" "outlet")
    --               (R.flow $ const Notebook <$> interval 30)
    --         nw' <- nw # R.connect
    --               (toOutlet "patch" "node1" "outlet")
    --               (toInlet "patch" "node2" "inlet")
    --         pure $ nw' /\ []
    --     collectedData `shouldContain`
    --       (InletData (toInlet "patch" "node2" "inlet") Notebook)
    --     collectedData' <- CollectData.channelsAfter
    --       (Milliseconds 100.0)
    --       nw'
    --       $ do
    --         _ <-
    --           nw' # R.disconnectAll
    --                   (toOutlet "patch" "node1" "outlet")
    --                   (toInlet "patch" "node2" "inlet")
    --         pure [ ]
    --     collectedData' `shouldContain`
    --       (OutletData (toOutlet "patch" "node1" "outlet") Notebook)
    --     collectedData' `shouldNotContain`
    --       (InletData (toInlet "patch" "node2" "inlet") Notebook)
    --     pure unit


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
