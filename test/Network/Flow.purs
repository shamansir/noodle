module RpdTest.Network.Flow
    ( spec ) where

import Prelude

import Data.Array (snoc, replicate, concat)
import Data.Either (Either, either, isRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List as List
import Data.Map as Map
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect, foreachE)
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import FRP.Event.Time (interval)
import Rpd ((</>), type (/->))
import Rpd as R
import Test.Spec (Spec, describe, it, pending, pending')
import Test.Spec.Assertions (shouldEqual, shouldContain, shouldNotContain)
import Unsafe.Coerce (unsafeCoerce)


infixl 6 snoc as +>


data Delivery
  = Damaged
  | Email
  | Letter
  | Parcel
  | TV
  | IKEAFurniture
  | Car
  | Notebook
  | Curse Int
  | Liver
  | Banana
  | Apple Int
  | Pills

derive instance genericDelivery :: Generic Delivery _

instance showDelivery :: Show Delivery where
  show = genericShow

instance eqDelivery :: Eq Delivery where
  eq = genericEq


type MyRpd = R.Rpd (R.Network Delivery)


spec :: Spec Unit
spec = do
  describe "data flow is functioning as expected" $ do

    -- INLETS --

    it "we receive no data from the network when it's empty" $ do
      (R.init "no-data" :: MyRpd)
        # withRpd \nw -> do
            collectedData <- nw #
              collectChannelsData (Milliseconds 100.0)
            collectedData `shouldEqual` []
            pure unit

      pure unit

    it "we receive no data from the inlet when it has no flow or default value" $ do
      let
        rpd :: MyRpd
        rpd =
          R.init "no-data"
            </> R.addPatch "patch"
            </> R.addNode (patchId 0) "node"
            </> R.addInlet (nodePath 0 0) "inlet"

      rpd # withRpd \nw -> do
              collectedData <- nw #
                collectChannelsData (Milliseconds 100.0)
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
            </> R.addNode (patchId 0) "node"
            </> R.addInlet (nodePath 0 0) "inlet"

      rpd # withRpd \nw -> do
          collectedData <- collectChannelsDataAfter
            (Milliseconds 100.0)
            nw
            $ do
              _ <- nw
                    # R.sendToInlet (inletPath 0 0 0) Parcel
                    </> R.sendToInlet (inletPath 0 0 0) Pills
                    </> R.sendToInlet (inletPath 0 0 0) (Curse 5)
              pure []
          collectedData `shouldEqual`
              [ InletData (inletPath 0 0 0) Parcel
              , InletData (inletPath 0 0 0) Pills
              , InletData (inletPath 0 0 0) (Curse 5)
              ]
          pure unit

    it "we receive the values from the data stream attached to the inlet" $ do
      let
        rpd :: MyRpd
        rpd =
          R.init "network"
            </> R.addPatch "patch"
            </> R.addNode (patchId 0) "node"
            </> R.addInlet (nodePath 0 0) "inlet"

      rpd # withRpd \nw -> do
          collectedData <- collectChannelsDataAfter
            (Milliseconds 100.0)
            nw
            $ do
              cancel :: R.Canceler <-
                nw # R.streamToInlet
                    (inletPath 0 0 0)
                    (R.flow $ const Pills <$> interval 30)
              pure [ cancel ]
          collectedData `shouldContain`
              (InletData (inletPath 0 0 0) Pills)
          pure unit

      pure unit

    pending "it is possible to manually cancel the streaming-to-inlet procedure"

    it "attaching several simultaneous streams to the inlet allows them to overlap" $ do
      let
        rpd :: MyRpd
        rpd =
          R.init "network"
            </> R.addPatch "patch"
            </> R.addNode (patchId 0) "node"
            </> R.addInlet (nodePath 0 0) "inlet"

      rpd # withRpd \nw -> do
          collectedData <- collectChannelsDataAfter
            (Milliseconds 100.0)
            nw
            $ do
              c1 <- nw # R.streamToInlet
                    (inletPath 0 0 0)
                    (R.flow $ const Pills <$> interval 20)
              c2 <- nw # R.streamToInlet
                    (inletPath 0 0 0)
                    (R.flow $ const Banana <$> interval 29)
              pure [ c1, c2 ]
          collectedData `shouldContain`
            (InletData (inletPath 0 0 0) Pills)
          collectedData `shouldContain`
            (InletData (inletPath 0 0 0) Banana)
          pure unit

      pure unit

    it "when the stream itself was stopped, values are not sent to the inlet anymore" $ do
      let
        rpd :: MyRpd
        rpd =
          R.init "network"
            </> R.addPatch "patch"
            </> R.addNode (patchId 0) "node"
            </> R.addInlet (nodePath 0 0) "inlet"

      rpd # withRpd \nw -> do
          collectedData <- collectChannelsDataAfter
            (Milliseconds 100.0)
            nw
            $ do
              cancel <- nw # R.streamToInlet
                  (inletPath 0 0 0)
                  (R.flow $ const Pills <$> interval 20)
              pure [ cancel ] -- `cancel` is called by `collectDataAfter`
          collectedData `shouldContain`
            (InletData (inletPath 0 0 0) Pills)
          collectedData' <- collectChannelsDataAfter
            (Milliseconds 100.0)
            nw
            $ pure []
          collectedData' `shouldNotContain`
            (InletData (inletPath 0 0 0) Pills)
          pure unit

    it "two different streams may work for different inlets" $ do
      let
        rpd :: MyRpd
        rpd =
          R.init "network"
            </> R.addPatch "patch"
            </> R.addNode (patchId 0) "node"
            </> R.addInlet (nodePath 0 0) "inlet1"
            </> R.addInlet (nodePath 0 0) "inlet2"

      rpd # withRpd \nw -> do
          collectedData <- collectChannelsDataAfter
            (Milliseconds 100.0)
            nw
            $ do
              c1 <-
                nw # R.streamToInlet
                  (inletPath 0 0 0)
                  (R.flow $ const Pills <$> interval 30)
              c2 <-
                nw # R.streamToInlet
                  (inletPath 0 0 1)
                  (R.flow $ const Banana <$> interval 25)
              pure [ c1, c2 ]
          collectedData `shouldContain`
            (InletData (inletPath 0 0 0) Pills)
          collectedData `shouldContain`
            (InletData (inletPath 0 0 1) Banana)
          pure unit

      pure unit

    it "same stream may produce values for several inlets" $ do
      let
        rpd :: MyRpd
        rpd =
          R.init "network"
            </> R.addPatch "patch"
            </> R.addNode (patchId 0) "node"
            </> R.addInlet (nodePath 0 0) "inlet1"
            </> R.addInlet (nodePath 0 0) "inlet2"

      rpd # withRpd \nw -> do
          collectedData <- collectChannelsDataAfter
            (Milliseconds 100.0)
            nw
            $ do
              let stream = R.flow $ const Banana <$> interval 25
              c1 <- nw # R.streamToInlet (inletPath 0 0 0) stream
              c2 <- nw # R.streamToInlet (inletPath 0 0 1) stream
              pure [ c1, c2 ]
          collectedData `shouldContain`
            (InletData (inletPath 0 0 0) Banana)
          collectedData `shouldContain`
            (InletData (inletPath 0 0 1) Banana)
          pure unit

      pure unit

    pending "sending data to the inlet triggers the processing function of the node"

    pending "receiving data from the stream triggers the processing function of the node"

    pending "default value of the inlet is sent to its flow when it's added"

    -- OULETS (same as for inlets) --

    -- LINKS <-> NODES --

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
          collectedData <- collectChannelsDataAfter
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
          collectedData <- collectChannelsDataAfter
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
          nw' /\ collectedData <- collectChannelsDataAfter'
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
          collectedData' <- collectChannelsDataAfter
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


data TraceItem d
  = InletData R.InletPath d
  | OutletData R.OutletPath d

type TracedFlow d = Array (TraceItem d)

derive instance genericTraceItem :: Generic (TraceItem d) _

instance showTraceItem :: Show d => Show (TraceItem d) where
  show = genericShow

instance eqTraceItem :: Eq d => Eq (TraceItem d) where
  eq = genericEq


patchId :: Int -> R.PatchId
patchId = R.PatchId

nodePath :: Int -> Int -> R.NodePath
nodePath = R.NodePath <<< R.PatchId

inletPath :: Int -> Int -> Int -> R.InletPath
inletPath patchId nodeId inletId = R.InletPath (nodePath patchId nodeId) inletId
-- inletPath = R.InletPath ?_ nodePath

outletPath :: Int -> Int -> Int -> R.OutletPath
outletPath patchId nodeId outletId = R.OutletPath (nodePath patchId nodeId) outletId
-- inletPath = R.InletPath ?_ nodePath


reportError :: R.RpdError -> Effect Unit
reportError = log <<< show


reportAndReturn :: forall a. a -> R.RpdError -> Effect a
reportAndReturn v err =
  reportError err >>= \_ -> pure v


runRpd :: forall a. (a -> Effect Unit) -> R.Rpd a -> Effect Unit
runRpd onSuccess rpd =
  R.run' reportError onSuccess rpd


extract :: forall a. a -> R.Rpd a -> Effect a
extract def rpd =
  R.run' (reportAndReturn def) pure rpd


withRpd
  :: forall d
   . (R.Network d -> Aff Unit)
  -> R.Rpd (R.Network d)
  -> Aff Unit
withRpd test rpd = do
  nw <- liftEffect $ getNetwork rpd
  test nw
  where
    --getNetwork :: R.Rpd d e -> R.RpdEff e (R.Network d e)
    getNetwork rpd = do
      nwTarget <- Ref.new $ R.emptyNetwork "f"
      _ <- runRpd (flip Ref.write $ nwTarget) rpd
      Ref.read nwTarget


collectChannelsData
  :: forall d
   . (Show d)
  => Milliseconds
  -> R.Network d
  -> Aff (TracedFlow d)
collectChannelsData period nw =
  collectChannelsDataAfter period nw $ pure []


collectChannelsDataAfter
  :: forall d
   . (Show d)
  => Milliseconds
  -> R.Network d
  -> R.Rpd (Array R.Canceler)
  -> Aff (TracedFlow d)
collectChannelsDataAfter period nw afterF = do
  collectChannelsDataAfter' period nw addNetwork >>= pure <<< snd
  where
    addNetwork = afterF >>= (\effs -> pure $ nw /\ effs)


collectChannelsDataAfter'
  :: forall d
   . (Show d)
  => Milliseconds
  -> R.Network d
  -> R.Rpd (R.Network d /\ Array R.Canceler)
  -> Aff (R.Network d /\ TracedFlow d)
collectChannelsDataAfter' period nw afterF = do
  target /\ cancelers <- liftEffect $ do
    target <- Ref.new []
    cancelers <- do
      let
        onInletData path {- source -} d = do
          curData <- Ref.read target
          Ref.write (curData +> InletData path d) target
          pure unit
        onOutletData path d = do
          curData <- Ref.read target
          Ref.write (curData +> OutletData path d) target
          pure unit
      cancelers <- R.subscribeChannelsData onOutletData onInletData nw
      pure $ foldCancelers cancelers
    pure $ target /\ cancelers
  nw' /\ userEffects <- liftEffect $ (extract (nw /\ [])) afterF
  delay period
  flow <- liftEffect $ do
    foreachE cancelers identity
    foreachE userEffects identity
    Ref.read target
  pure $ nw' /\ flow
  where
    foldCancelers
      :: (R.OutletPath /-> R.Canceler) /\ (R.InletPath /-> R.Canceler)
      -> Array R.Canceler
    foldCancelers (outletsMap /\ inletsMap) =
      List.toUnfoldable $ Map.values outletsMap <> Map.values inletsMap


-- logOrExec
--   :: forall a. Either R.RpdError (Effect a) -> Effect a
-- logOrExec effE =
--   either (log <<< show) identity effE
