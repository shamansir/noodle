module RpdTest.Flow.Inlets
    ( spec
    ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))

import FRP.Event.Time (interval)

import Rpd.API.Action.Sequence ((</>))
import Rpd.API.Action.Sequence (init) as Actions
import Rpd.API.Action.Sequence as R
import Rpd.Network (empty) as Network
import Rpd.Path (toPatch, toNode, toInlet)
import Rpd.Util (flow) as R

import Test.Spec (Spec, it, pending, pending')
import Test.Spec.Assertions (shouldEqual, shouldContain, shouldNotContain)

import RpdTest.Helper (TraceItem(..))
import RpdTest.Helper (channelsAfter) as CollectData
import RpdTest.Flow.Base (Delivery(..), Pipe(..), Node(..), Actions, myToolkit)


{- ======================================= -}
{- =============== INLETS ================ -}
{- ======================================= -}

spec :: Spec Unit
spec = do

  -- INLETS --

  it "we receive no data from the inlet when it has no flow or default value" $ do
    let
      structure :: Actions
      structure =
        Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node" Empty
          </> R.addInlet (toNode "patch" "node") "inlet" Pass

    _ /\ collectedData <-
        CollectData.channelsAfter
          (Milliseconds 100.0)
          myToolkit
          (Network.empty "network")
          structure
    collectedData `shouldEqual` []
    pure unit

  pending "we receive the default value of the inlet just when it was set"

  it "we receive the data sent directly to the inlet" $ do
    let
      structure :: Actions
      structure =
        Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node" Empty
          </> R.addInlet (toNode "patch" "node") "inlet" Pass
      firstInlet = toInlet "patch" "node" "inlet"

    _ /\ collectedData <-
      CollectData.channelsAfter
          (Milliseconds 100.0)
          myToolkit
          (Network.empty "network")
          $ structure
              </> R.sendToInlet firstInlet Parcel
              </> R.sendToInlet firstInlet Pills
              </> R.sendToInlet firstInlet (Curse 5)

    collectedData `shouldEqual`
        [ InletData firstInlet Parcel
        , InletData firstInlet Pills
        , InletData firstInlet (Curse 5)
        ]

  it "we receive the values from the data stream attached to the inlet" $ do
    let
      structure :: Actions
      structure =
        Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node" Empty
          </> R.addInlet (toNode "patch" "node") "inlet" Pass

    _ /\ collectedData <- CollectData.channelsAfter
      (Milliseconds 100.0)
      myToolkit
      (Network.empty "network")
      $ structure
         </> R.streamToInlet
                (toInlet "patch" "node" "inlet")
                (R.flow $ const Pills <$> interval 30)

    collectedData `shouldContain`
        (InletData (toInlet "patch" "node" "inlet") Pills)
    pure unit

  pending "it is possible to manually cancel the streaming-to-inlet procedure"

  it "attaching several simultaneous streams to the inlet allows them to overlap" $ do
    let
      structure :: Actions
      structure =
        Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node" Empty
          </> R.addInlet (toNode "patch" "node") "inlet" Pass
      firstInlet = toInlet "patch" "node" "inlet"

    _ /\ collectedData <- CollectData.channelsAfter
      (Milliseconds 100.0)
      myToolkit
      (Network.empty "network")
      $ structure
            </> R.streamToInlet
                  (toInlet "patch" "node" "inlet")
                  (R.flow $ const Pills <$> interval 20)
            </> R.streamToInlet
                  (toInlet "patch" "node" "inlet")
                  (R.flow $ const Banana <$> interval 29)

    collectedData `shouldContain`
      (InletData (toInlet "patch" "node" "inlet") Pills)
    collectedData `shouldContain`
      (InletData (toInlet "patch" "node" "inlet") Banana)

    pure unit

  -- TODO: could be replaced, since for now user has no control over stopping the stream
  pending' "when the stream itself was stopped, values are not sent to the inlet anymore" $ do
    let
      structure :: Actions
      structure =
        Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node" Empty
          </> R.addInlet (toNode "patch" "node") "inlet" Pass

    _ /\ collectedData <- CollectData.channelsAfter
      (Milliseconds 100.0)
      myToolkit
      (Network.empty "network")
      $ structure
          </> R.streamToInlet
                (toInlet "patch" "node" "inlet")
                (R.flow $ const Pills <$> interval 20)

    collectedData `shouldContain`
      (InletData (toInlet "patch" "node" "inlet") Pills)
    -- collectedData' <- CollectData.channelsAfter
    --   (Milliseconds 100.0)
    --   nw
    --   $ pure []
    -- collectedData' `shouldNotContain`
    --   (InletData (toInlet "patch" "node" "inlet") Pills)
    pure unit

  it "two different streams may work for different inlets" $ do
    let
      structure :: Actions
      structure =
        Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node" Empty
          </> R.addInlet (toNode "patch" "node") "for-pills" Pass
          </> R.addInlet (toNode "patch" "node") "for-bananas" Pass

    _ /\ collectedData <- CollectData.channelsAfter
      (Milliseconds 100.0)
      myToolkit
      (Network.empty "network")
      $ structure
          </> R.streamToInlet
                (toInlet "patch" "node" "for-pills")
                (R.flow $ const Pills <$> interval 30)
          </> R.streamToInlet
                (toInlet "patch" "node" "for-bananas")
                (R.flow $ const Banana <$> interval 25)
                
    collectedData `shouldContain`
      (InletData (toInlet "patch" "node" "for-pills") Pills)
    collectedData `shouldContain`
      (InletData (toInlet "patch" "node" "for-bananas") Banana)
    pure unit

  it "same stream may produce values for several inlets" $ do
    let
      structure :: Actions
      structure =
        Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node" Empty
          </> R.addInlet (toNode "patch" "node") "inlet1" Pass
          </> R.addInlet (toNode "patch" "node") "inlet2" Pass
      stream = R.flow $ const Banana <$> interval 25

    _ /\ collectedData <- CollectData.channelsAfter
      (Milliseconds 100.0)
      myToolkit
      (Network.empty "network")
      $ structure
          </> R.streamToInlet (toInlet "patch" "node" "inlet1") stream
          </> R.streamToInlet (toInlet "patch" "node" "inlet2") stream
    collectedData `shouldContain`
      (InletData (toInlet "patch" "node" "inlet1") Banana)
    collectedData `shouldContain`
      (InletData (toInlet "patch" "node" "inlet2") Banana)
    pure unit

  pending "sending data to the inlet triggers the processing function of the node"

  pending "receiving data from the stream triggers the processing function of the node"

  pending "default value of the inlet is sent to its flow when it's added"
