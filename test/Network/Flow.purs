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

import FRP (FRP)
import FRP.Event (fold) as Event
import FRP.Event.Time (interval)

import Rpd as R
import Rpd ((~>), type (/->))
-- import Rpd.Flow
--   ( flow
--   , subscribeAll, subscribeTop
--   , Subscribers, Subscriber, Canceler
--   ) as R

import Test.Spec (Spec, describe, it, pending, pending')
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Util (TestAffE, runWith)


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


spec :: forall e. Spec (TestAffE e) Unit
spec = do
  describe "flow is defined before running the system" $ do

    it "we receive no data from the network when it's empty" $ do
      _ <- (R.init "no-data" :: R.Rpd Delivery e)
           # withRpd \nw -> do
                collectedData <- collectData nw (Milliseconds 100.0)
                collectedData `shouldEqual` []
                pure unit

      pure unit

    it "we receive no data from the inlet when it has no flow or default value" $ do
      let
        rpd :: R.Rpd Delivery e
        rpd =
          R.init "no-data"
            ~> R.addPatch "foo"
            ~> R.addNode (patchId 0) "test1"
            ~> R.addNode (patchId 0) "test2"
            ~> R.addInlet (nodePath 0 0) "label"

      _ <- rpd # withRpd \nw -> do
                  collectedData <- collectData nw (Milliseconds 100.0)
                  collectedData `shouldEqual` []
                  pure unit

      pure unit

    it "we receive the data sent directly to the inlet" $ do
      let
        rpd :: R.Rpd Delivery e
        rpd =
          R.init "no-data"
            ~> R.addPatch "foo"
            ~> R.addNode (patchId 0) "test1"
            ~> R.addNode (patchId 0) "test2"
            ~> R.addInlet (nodePath 0 0) "label"

      _ <- rpd # withRpd \nw -> do
          collectedData <- (collectDataAfter
            (\_ -> do
              _ <- rpd ~> R.sendToInlet (inletPath 0 0 0) Pills
              pure unit)
            nw
            (Milliseconds 100.0))
          -- _ <- liftEff $ (rpd ~> R.sendToInlet (inletPath 0 0 0) Pills)
          collectedData `shouldNotEqual` []
          pure unit

      pure unit


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
  -> R.Rpd d e
  -> Aff (TestAffE e) Unit
withRpd test rpd = do
  nw <- liftEff $ getNetwork rpd
  test nw
  where
    getNetwork :: R.Rpd d e -> R.RpdEff e (R.Network d e)
    getNetwork rpd = do
      nwTarget <- newRef $ R.emptyNetwork "f"
      _ <- R.run (log <<< show) (writeRef nwTarget) rpd
      readRef nwTarget


collectData
  :: forall d e
   . (Show d)
  => R.Network d e
  -> Milliseconds
  -> Aff (TestAffE e) (TracedFlow d)
collectData = collectDataAfter (const $ pure unit)


collectDataAfter
  :: forall d e
   . (Show d)
  => (Unit -> R.RpdEff e Unit)
  -> R.Network d e
  -> Milliseconds
  -> Aff (TestAffE e) (TracedFlow d)
collectDataAfter afterF nw period = do
  target /\ cancelers <- liftEff $ do
    target <- newRef []
    cancelers <- do
      let
        onInletData path {- source -} d = do
          curData <- readRef target
          _ <- writeRef target $ curData +> InletData path d
          pure unit
        onOutletData path d = do
          curData <- readRef target
          _ <- writeRef target $ curData +> OutletData path d
          pure unit
      cancelers <- R.subscribeAllData onOutletData onInletData nw
      pure $ foldCancelers cancelers
    pure $ target /\ cancelers
  _ <- liftEff $ afterF unit
  delay period
  liftEff $ do
    cancelSubs cancelers
    readRef target
  where
    foldCancelers :: ((R.OutletPath /-> R.Canceler e) /\ (R.InletPath /-> R.Canceler e)) -> Array (R.Canceler e)
    foldCancelers (outletsMap /\ inletsMap) =
      List.toUnfoldable $ Map.values outletsMap <> Map.values inletsMap

cancelSub :: forall e. R.Canceler e -> R.RpdEff e Unit
cancelSub canceler = do
  _ <- canceler
  pure unit


cancelSubs :: forall e. Array (R.Canceler e) -> R.RpdEff e Unit
cancelSubs cancelers =
  foreachE cancelers cancelSub
