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
      "b" `shouldEqual` "b"
      -- let
      --   network :: R.Network Delivery
      --   network =
      --     R.network [
      --       R.patch "Test 0001" [
      --         R.node "Specimen"
      --           [ R.inlet "foo" ]
      --           [ ]
      --       ]
      --     ]

      -- runWith network
      --   \nw ->
      --     do
      --       collectedData <- collectData nw (Milliseconds 100.0)
      --       collectedData `shouldEqual` []
      --       pure unit

