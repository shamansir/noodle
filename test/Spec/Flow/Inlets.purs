module Noodle.Test.Spec.Flow.Inlets
    ( spec
    ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))

import FRP.Event.Time (interval)

import Noodle.API.Action.Sequence ((</>))
import Noodle.API.Action.Sequence (init) as Actions
import Noodle.API.Action.Sequence as R
import Noodle.Network (empty) as Network
import Noodle.Path (toPatch, toNode, toInlet)
import Noodle.Util (flow) as R

import Effect.Class (liftEffect)

import Test.Spec (Spec, it, pending, pending', itOnly)
import Test.Spec.Assertions (shouldEqual, shouldContain, shouldNotContain)

import Noodle.Test.Util.Trace (TraceItem(..))
import Noodle.Test.Util.Trace (channelsAfter) as CollectData
import Noodle.Test.Spec.Flow.Base (Delivery(..), Pipe(..), Node(..), Actions, mySequencer)


{- ======================================= -}
{- =============== INLETS ================ -}
{- ======================================= -}

spec :: Spec Unit
spec = do

  -- INLETS --

  it "we receive no data from the inlet when it has no flow" $ do
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
          mySequencer
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
          mySequencer
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
      mySequencer
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
      mySequencer
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
  pending' "when there are no incoming streams anymore, values are not sent to the inlet" $ do
    let
      structure :: Actions
      structure =
        Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node" Empty
          </> R.addInlet (toNode "patch" "node") "inlet" Pass

    _ /\ collectedData <- CollectData.channelsAfter
      (Milliseconds 100.0)
      mySequencer
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
      mySequencer
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
      mySequencer
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

  pending "on connection, latest value of the outlet is sent to the inlet's flow"

  pending "if default value of the inlet is specified, then it is sent to its flow when node was created"

  pending "if default value of the inlet is specified, the inlet switches back to this value when everyone is diconnected"

  pending "when we connect several streams to the inlet (and it allows such connections), the data is received in parallel from all of them"

  pending "when we connect new stream to the inlet and it is already connected (and in 'one-connection-only' mode), the data from the newly-connected stream replaces the data from the previous one"

