module RpdTest.Flow.Inlets
    ( spec
    ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))

import FRP.Event.Time (interval)

import Rpd.API ((</>))
import Rpd.API as R
import Rpd.Path (toPatch, toNode, toInlet)
import Rpd.Util (flow, Canceler) as R

import Test.Spec (Spec, it, pending)
import Test.Spec.Assertions (shouldEqual, shouldContain, shouldNotContain)

import RpdTest.Util (withRpd)
import RpdTest.CollectData (TraceItem(..))
import RpdTest.CollectData as CollectData
import RpdTest.Flow.Base (MyRpd, Delivery(..))


{- ======================================= -}
{- =============== INLETS ================ -}
{- ======================================= -}

spec :: Spec Unit
spec = do
  pending "we are able to subscribe some specific inlet in the network"

  pending "we are able to subscribe some specific outlet in the network"

  pending "we are able to subscribe some specific node in the network"

  -- INLETS --

  it "we receive no data from the inlet when it has no flow or default value" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "no-data"
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node"
          </> R.addInlet (toNode "patch" "node") "inlet"

    rpd # withRpd \nw -> do
            collectedData <- nw #
              CollectData.channels (Milliseconds 100.0)
            collectedData `shouldEqual` []
            pure unit

    pure unit

  pending "we receive the default value of the inlet just when it was set"

  it "we receive the data sent directly to the inlet" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node"
          </> R.addInlet (toNode "patch" "node") "inlet"

    rpd # withRpd \nw -> do
        let firstInlet = toInlet "patch" "node" "inlet"
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            _ <- nw
                   #  R.sendToInlet firstInlet Parcel
                  </> R.sendToInlet firstInlet Pills
                  </> R.sendToInlet firstInlet (Curse 5)
            pure []
        collectedData `shouldEqual`
            [ InletData firstInlet Parcel
            , InletData firstInlet Pills
            , InletData firstInlet (Curse 5)
            ]
        pure unit

  it "we receive the values from the data stream attached to the inlet" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node"
          </> R.addInlet (toNode "patch" "node") "inlet"

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            cancel :: R.Canceler <-
              nw # R.streamToInlet
                  (toInlet "patch" "node" "inlet")
                  (R.flow $ const Pills <$> interval 30)
            pure [ cancel ]
        collectedData `shouldContain`
            (InletData (toInlet "patch" "node" "inlet") Pills)
        pure unit

    pure unit

  pending "it is possible to manually cancel the streaming-to-inlet procedure"

  it "attaching several simultaneous streams to the inlet allows them to overlap" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node"
          </> R.addInlet (toNode "patch" "node") "inlet"

    rpd # withRpd \nw -> do
        let firstInlet = toInlet "patch" "node" "inlet"
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            c1 <- nw # R.streamToInlet
                  (toInlet "patch" "node" "inlet")
                  (R.flow $ const Pills <$> interval 20)
            c2 <- nw # R.streamToInlet
                  (toInlet "patch" "node" "inlet")
                  (R.flow $ const Banana <$> interval 29)
            pure [ c1, c2 ]
        collectedData `shouldContain`
          (InletData (toInlet "patch" "node" "inlet") Pills)
        collectedData `shouldContain`
          (InletData (toInlet "patch" "node" "inlet") Banana)
        pure unit

    pure unit

  it "when the stream itself was stopped, values are not sent to the inlet anymore" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node"
          </> R.addInlet (toNode "patch" "node") "inlet"

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            cancel <- nw # R.streamToInlet
                (toInlet "patch" "node" "inlet")
                (R.flow $ const Pills <$> interval 20)
            pure [ cancel ] -- `cancel` is called by `collectDataAfter`
        collectedData `shouldContain`
          (InletData (toInlet "patch" "node" "inlet") Pills)
        collectedData' <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ pure []
        collectedData' `shouldNotContain`
          (InletData (toInlet "patch" "node" "inlet") Pills)
        pure unit

  it "two different streams may work for different inlets" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node"
          </> R.addInlet (toNode "patch" "node") "for-pills"
          </> R.addInlet (toNode "patch" "node") "for-bananas"

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            c1 <-
              nw # R.streamToInlet
                (toInlet "patch" "node" "for-pills")
                (R.flow $ const Pills <$> interval 30)
            c2 <-
              nw # R.streamToInlet
                (toInlet "patch" "node" "for-bananas")
                (R.flow $ const Banana <$> interval 25)
            pure [ c1, c2 ]
        collectedData `shouldContain`
          (InletData (toInlet "patch" "node" "for-pills") Pills)
        collectedData `shouldContain`
          (InletData (toInlet "patch" "node" "for-bananas") Banana)
        pure unit

    pure unit

  it "same stream may produce values for several inlets" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node"
          </> R.addInlet (toNode "patch" "node") "inlet1"
          </> R.addInlet (toNode "patch" "node") "inlet2"

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            let stream = R.flow $ const Banana <$> interval 25
            c1 <- nw # R.streamToInlet (toInlet "patch" "node" "inlet1") stream
            c2 <- nw # R.streamToInlet (toInlet "patch" "node" "inlet2") stream
            pure [ c1, c2 ]
        collectedData `shouldContain`
          (InletData (toInlet "patch" "node" "inlet1") Banana)
        collectedData `shouldContain`
          (InletData (toInlet "patch" "node" "inlet2") Banana)
        pure unit

    pure unit

  pending "sending data to the inlet triggers the processing function of the node"

  pending "receiving data from the stream triggers the processing function of the node"

  pending "default value of the inlet is sent to its flow when it's added"
