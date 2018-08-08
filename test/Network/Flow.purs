module RpdTest.Network.Flow
    ( spec ) where

import Prelude

import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)

import Data.Array (fromFoldable, concatMap, snoc, replicate, concat)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..), fromMaybe)

import FRP (FRP)
import FRP.Event (fold) as Event
import FRP.Event.Time (interval)

import Rpd as R
import Rpd ((~>))
-- import Rpd.Flow
--   ( flow
--   , subscribeAll, subscribeTop
--   , Subscribers, Subscriber, Canceler
--   ) as R

import Test.Spec (Spec, describe, it, pending, pending')
import Test.Spec.Assertions (shouldEqual)
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
      "a" `shouldEqual` "a"
      pure unit

    it "we receive no data from the inlet when it has no flow or default value" $ do
      let
        rpd :: R.Rpd Delivery e
        rpd =
          R.init "t"
            # R.addPatch "foo"
            ~> R.addNode (R.PatchId 0) "test1"
            ~> R.addNode (R.PatchId 0) "test2"

      nw <- R.run rpd
      collectedData <- collectData nw (Milliseconds 100.0)
      collectedData `shouldEqual` []
      pure unit


data TraceItem d
  = InletData R.InletPath d
  | OutletData R.OutletPath d

type TracedFlow d = Array (TraceItem d)


collectData
  :: forall d e
   . (Show d)
  => R.Network d e
  -> Milliseconds
  -> Aff (TestAffE e) (TracedFlow d)
collectData nw period = do
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
      pure cancelers
    pure $ target /\ cancelers
  delay period
  liftEff $ do
    cancelSubs cancelers
    readRef target


cancelSub :: forall e. R.Canceler e -> Eff (frp :: FRP | e) Unit
cancelSub canceler = do
  _ <- canceler
  pure unit


cancelSubs :: forall e. Array (R.Canceler e) -> Eff (frp :: FRP | e) Unit
cancelSubs cancelers =
  foreachE cancelers cancelSub
