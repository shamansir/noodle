module RpdTest.Network.Flow
    ( spec ) where

import Prelude

import Control.Monad.Aff (Aff, delay, makeAff)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)

import Data.Array (fromFoldable, concatMap, snoc, replicate, concat)
import Data.List (List)
import Data.List as List
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (fold)
import Data.Traversable (sequence)

import FRP (FRP)
import FRP.Event (fold) as Event
import FRP.Event.Time (interval)

import Rpd as R
import Rpd ((</>), type (/->))
-- import Rpd.Flow
--   ( flow
--   , subscribeAll, subscribeTop
--   , Subscribers, Subscriber, Canceler
--   ) as R

import Test.Spec (Spec, describe, it, pending, pending')
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Util (TestAffE, runWith)


import Data.Either
import Control.Monad.Eff.Unsafe (unsafePerformEff)

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


type MyRpd e = R.Rpd e (R.Network Delivery e)


spec :: forall e. Spec (TestAffE e) Unit
spec = do
  describe "data flow is functioning as expected" $ do

    -- INLETS --

    it "we receive no data from the network when it's empty" $ do
      (R.init "no-data" :: MyRpd e)
        # withRpd \nw -> do
            collectedData <- nw # collectData (Milliseconds 100.0)
            collectedData `shouldEqual` []
            pure unit

      pure unit

    it "we receive no data from the inlet when it has no flow or default value" $ do
      let
        rpd :: MyRpd e
        rpd =
          R.init "no-data"
            </> R.addPatch "foo"
            </> R.addNode (patchId 0) "test1"
            </> R.addInlet (nodePath 0 0) "label"

      rpd # withRpd \nw -> do
              collectedData <- nw # collectData (Milliseconds 100.0)
              collectedData `shouldEqual` []
              pure unit

      pure unit

    pending "we receive the default value of the inlet just when it was set"

    it "we receive the data sent directly to the inlet" $ do
      let
        rpd :: MyRpd e
        rpd =
          R.init "no-data"
            </> R.addPatch "foo"
            </> R.addNode (patchId 0) "test1"
            </> R.addInlet (nodePath 0 0) "label"

      rpd # withRpd \nw -> do
          collectedData <- collectDataAfter
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
        rpd :: MyRpd e
        rpd =
          R.init "no-data"
            </> R.addPatch "foo"
            </> R.addNode (patchId 0) "test1"
            </> R.addInlet (nodePath 0 0) "label"

      rpd # withRpd \nw -> do
          collectedData <- collectDataAfter
            (Milliseconds 100.0)
            nw
            $ do
              cancel <-
                nw # R.streamToInlet
                    (inletPath 0 0 0)
                    (R.flow $ const Pills <$> interval 30)
              pure $ postpone [ cancel ]
          collectedData `shouldEqual`
              (replicate 3 $ InletData (inletPath 0 0 0) Pills)
          pure unit

      pure unit

    pending "it is possible to manually cancel the streaming-to-inlet procedure"

    it "attaching several simultaneous streams to the inlet allows them to overlap" $ do
      let
        rpd :: MyRpd e
        rpd =
          R.init "no-data"
            </> R.addPatch "foo"
            </> R.addNode (patchId 0) "test1"
            </> R.addInlet (nodePath 0 0) "label"

      rpd # withRpd \nw -> do
          collectedData <- collectDataAfter
            (Milliseconds 121.0)
            nw
            $ do
              c1 <- nw # R.streamToInlet (inletPath 0 0 0) (R.flow $ const Pills <$> interval 30)
              c2 <- nw # R.streamToInlet (inletPath 0 0 0) (R.flow $ const Banana <$> interval 25)
              pure $ postpone [ c1, c2 ]
          collectedData `shouldEqual`
              ((concat $ replicate 3 $
                  [ InletData (inletPath 0 0 0) Banana
                  , InletData (inletPath 0 0 0) Pills
                  ]
                ) +> InletData (inletPath 0 0 0) Banana)
          pure unit

      pure unit

    pending "when the stream itself was stopped, values are not sent to the inlet anymore"

    pending "two different streams may work for different inlets"

    pending "same stream may work for several inlets"

    pending "sending data to the inlet triggers the processing function of the node"

    pending "receiving data from the stream triggers the processing function of the node"

    -- OULETS --

    -- LINKS <-> NODES --

    pending "connecting some outlet to some inlet makes data flow from this outlet to this inlet"

    pending "disconnecting some outlet from some inlet makes data flow between them stop"


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


withRpd
  :: forall d e
   . (R.Network d e -> Aff (TestAffE e) Unit)
  -> R.Rpd e (R.Network d e)
  -> Aff (TestAffE e) Unit
withRpd test rpd = do
  nw <- liftEff $ getNetwork rpd
  test nw
  where
    --getNetwork :: R.Rpd d e -> R.RpdEff e (R.Network d e)
    getNetwork rpd = do
      nwTarget <- newRef $ R.emptyNetwork "f"
      _ <- R.run (log <<< show) (writeRef nwTarget) rpd
      readRef nwTarget


collectData
  :: forall d e
   . (Show d)
  => Milliseconds
  -> R.Network d e
  -> Aff (TestAffE e) (TracedFlow d)
collectData period nw = collectDataAfter period nw $ pure []


collectDataAfter
  :: forall d e
   . (Show d)
  => Milliseconds
  -> R.Network d e
  -> R.RpdEff e (Array (R.Canceler e))
  -> Aff (TestAffE e) (TracedFlow d)
collectDataAfter period nw afterF = do
  target /\ cancelers <- liftEff $ do
    target <- newRef []
    cancelers <- do
      let
        onInletData path {- source -} d = do
          curData <- readRef target
          writeRef target $ curData +> InletData path d
          pure unit
        onOutletData path d = do
          curData <- readRef target
          writeRef target $ curData +> OutletData path d
          pure unit
      cancelers <- R.subscribeAllData onOutletData onInletData nw
      pure $ foldCancelers cancelers
    pure $ target /\ cancelers
  userEffects <- liftEff $ afterF
  delay period
  liftEff $ do
    foreachE cancelers id
    foreachE userEffects id
    readRef target
  where
    foldCancelers :: ((R.OutletPath /-> R.Canceler e) /\ (R.InletPath /-> R.Canceler e)) -> Array (R.Canceler e)
    foldCancelers (outletsMap /\ inletsMap) =
      List.toUnfoldable $ Map.values outletsMap <> Map.values inletsMap


postpone
  :: forall e
   . Array (Either R.RpdError (R.RpdEff e Unit))
  -> Array (R.RpdEff e Unit)
postpone src =
  map logOrExec src
  where
    logOrExec cancelerE =
      either (log <<< show) id cancelerE
