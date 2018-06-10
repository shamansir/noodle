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
import Data.Tuple.Nested ((/\))

import FRP (FRP)
import FRP.Event (fold) as Event
import FRP.Event.Time (interval)

import Rpd as R
import Rpd.Flow (flow, subscribeAll, Subscribers, Subscriber, Canceler) as R

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

    it "we receive no data from the inlet when it has no flow or default value" $ do
      let
        network :: R.Network Delivery
        network =
          R.network [
            R.patch "Test 0001" [
              R.node "Specimen"
                [ R.inlet "foo" ]
                [ ]
            ]
          ]
      runWith network
        \nw ->
          do
            collectedData <- collectData nw (Milliseconds 100.0)
            collectedData `shouldEqual` []
            pure unit

    it "we receive no data from the outlet when it has no flow" $ do
      let
        network :: R.Network Delivery
        network =
          R.network [
            R.patch "Test 0002" [
              R.node "Specimen"
                [ ]
                [ R.outlet "bar" ]
            ]
          ]
      runWith network
        \nw ->
          do
            collectedData <- collectData nw (Milliseconds 100.0)
            collectedData `shouldEqual` []
            pure unit

    pending' "we're able to collect the default value from the inlet" $ do
    -- or we shouldn't be able to collect it, and it's the value rendered from inlet data and sent on connection first, and also when there's no flow in outlet/inlet??
      let
        network =
          R.network [
            R.patch "Test 0001" [
              R.node "Recepient"
                [ R.inletWithDefault "mouth" Pills ]
                [ ]
            ]
          ]
      runWith network
        \nw ->
          do
            collectedData <- collectData nw (Milliseconds 100.0)
            collectedData `shouldEqual` [ InletData (inletPath 0 0 0) Pills ]
            pure unit

    it "we're able to collect the data from the flow attached to the inlet" $ do
      let
        network =
          R.network [
            R.patch "Test 0001" [
              R.node "Recepient"
                [ R.inlet' "mouth" $ R.flow $ const Pills <$> interval 100 ]
                [ ]
            ]
          ]
      runWith network
        \nw ->
          do
            collectedData <- collectData nw (Milliseconds 600.0)
            collectedData `shouldEqual` (replicate 5 $ InletData (inletPath 0 0 0) Pills)
            pure unit

    it "we're able to collect the data from the flow attached to the outlet" $ do
      let
        network =
          R.network [
            R.patch "Test 0001" [
              R.node "Producer"
                [ ]
                [ R.outlet' "factory" $ R.flow $ const Banana <$> interval 100 ]
            ]
          ]
      runWith network
        \nw ->
          do
            collectedData <- collectData nw (Milliseconds 600.0)
            collectedData `shouldEqual` (replicate 5 $ OutletData (outletPath 0 0 0) Banana)
            pure unit

    pending' "connecting the outlet to the inlet actually sends the data" $ do
      -- TODO: test it for the "after" case below first, then bring back
      let
        factory = R.outlet' "factory" $ R.flow $ const Banana <$> interval 100
        consume = R.inlet "consumer"
        producerNode =
          R.node "Producer"
            [ ]
            [ factory ]
        receiverNode =
          R.node "Receiver"
            [ consume ]
            [ ]
        patch =
          R.patch "Test 0001"
            [ producerNode
            , receiverNode
            ]
        patch' = R.connect (outletPath 0 1 0) (inletPath 0 0 0) patch
        --network = R.network [ patch' ]
        network = R.network [ patch ]
      runWith network
        \nw ->
          do
            collectedData <- collectData nw (Milliseconds 600.0)
            collectedData `shouldEqual`
                (concat $ replicate 5 $
                  [ OutletData (outletPath 0 0 0) Banana
                  , InletData (inletPath 0 1 0) Banana
                  ]
                )
            pure unit

    -- TODO: also try subscribing to the specific inlet / outlet for all the cases where it has sense
    it "subscriber receives all the data flowing in the complex network" do
      runWith postNetwork
        \nw ->
          do
            collectedData <- collectData nw (Milliseconds 1000.0)
            -- liftEff $ log $ "collected: " <> show collectedData
            collectedData `shouldEqual`
              [ OutletData (outletPath 0 1 1) Banana
              , OutletData (outletPath 0 0 2) (Apple 1)
              , InletData  (inletPath 0 0 5)  (Curse 1)
              , OutletData (outletPath 0 1 1) Banana
              , OutletData (outletPath 0 1 1) Banana
              , OutletData (outletPath 0 0 2) (Apple 2)
              , InletData  (inletPath 0 0 5)  (Curse 2)
              , OutletData (outletPath 0 1 1) Banana
              , OutletData (outletPath 0 0 1) Letter
              , OutletData (outletPath 0 1 1) Banana
              , OutletData (outletPath 0 0 2) (Apple 3)
              , InletData  (inletPath 0 0 5)  (Curse 3)
              , OutletData (outletPath 0 1 1) Banana
              , OutletData (outletPath 0 1 1) Banana
              , OutletData (outletPath 0 0 2) (Apple 4)
              , InletData  (inletPath 0 0 5)  (Curse 4)
              , OutletData (outletPath 0 1 1) Banana
              , OutletData (outletPath 0 1 1) Banana
              ]
            pure unit

  describe "flow is defined after running the system" do
    pending "TODO"

  -- describe "subscribing to the data flow" do
  --     it "receives the data from events" do

  -- describe "connecting channels after running the system" do
  --   pure unit
  -- describe "disconnecting channels after running the system" do
  --   pure unit
  -- describe "manually sending data to the channels after running the system" do
  --   pure unit
  -- describe "manually sending delayed data to the channels after running the system" do
  --   --   delay (Milliseconds 100.0)
  --   pure unit
  -- describe "adding nodes after creation" do
  --   pure unit
  -- describe "deleting nodes after creation" do
  --   pure unit
  where
    outletPath :: Int -> Int -> Int -> R.OutletPath
    outletPath a b c = R.OutletPath (R.NodePath (R.PatchId a) b) c
    inletPath :: Int -> Int -> Int -> R.InletPath
    inletPath a b c = R.InletPath (R.NodePath (R.PatchId a) b) c


postServiceNode :: R.LazyNode Delivery
postServiceNode =
  R.node "Post Service"
    [ R.inlet "Mezozon"
    , R.inletWithDefault "Mr. Schtirlitz" Liver
    , R.inlet' "Buyer UG" $ R.flow $ const Pills <$> interval 5000
    , R.inlet "Mrs. Black Widow"
    , R.inlet "eBay"
    , R.inlet' "kleinanzeige"
        $ R.flow $ map Curse $ Event.fold (\_ n -> n + 1) (interval 200) 0
    ]
    [ R.outlet "Postman 1"
    , R.outlet' "Postman 2" $ R.flow
        $ const Letter <$> (interval 500)
    , R.outlet' "Truck 1" $ R.flow
        $ map Apple
        $ Event.fold (\_ n -> n + 1) (interval 200) 0
    ]


upsNode :: R.LazyNode Delivery
upsNode =
  R.node "UPS"
    [ R.inlet "Incoming 1"
    , R.inlet "Incoming 2"
    ]
    [ R.outlet "Outgoing 1"
    , R.outlet' "Outgoing 2"
        $ R.flow $ const Banana <$> interval 100
    ]


recipientNode :: R.LazyNode Delivery
recipientNode =
  R.node "Recepient"
    [ R.inlet "hands"
    , R.inlet "postbox"
    , R.inlet "pocket"
    ]
    [ R.outlet "debts"
    , R.outlet "taxes"
    ]


postNetwork :: R.Network Delivery
postNetwork =
  R.network
    [ R.patch "Post Network"
      [ postServiceNode
      , upsNode
      , recipientNode
      -- , R.processWith processF $ node "2"
      ] -- >>> connect (patch.getNode 0) "a" (patch.getNode 1) "b"
    ]
  -- where
  --   processF inputs | Map.isEmpty inputs = Map.empty
  --   processF inputs | Map.member "d" inputs =
  --     Map.singleton "c" $ fromMaybe Damaged $ Map.lookup "d" inputs
  --   processF inputs = Map.empty


-- collectData
--   :: forall e
--    . R.Network MyData
--   -> Milliseconds
--   -> Aff (TestAffE e) (Array (R.OutletPath /\ MyData))
-- collectData nw period = do
--   target <- liftEff $ newRef []
--   cancelers <- liftEff $ do
--     let
--       onInletData :: R.InletPath -> R.DataSource MyData -> MyData -> Eff (TestAffE e) Unit
--       onInletData path source d = do
--         log $ show path <> show d
--       onOutletData path d = do
--         curData <- readRef target
--         _ <- writeRef target $ (path /\ d) : curData
--         pure unit
--       subscribers = R.subscribeAll onInletData onOutletData nw
--     performSubs subscribers
--   delay period
--   liftEff $ cancelSubs cancelers
--   liftEff $ readRef target


data TraceItem d
  = InletData R.InletPath d
  | OutletData R.OutletPath d

type TracedFlow d = Array (TraceItem d)

derive instance genericTraceItem :: Generic (TraceItem d) _

instance showTraceItem :: Show d => Show (TraceItem d) where
  show = genericShow

instance eqTraceItem :: Eq d => Eq (TraceItem d) where
  eq = genericEq


collectData
  :: forall d e
   . (Show d)
  => R.Network d
  -> Milliseconds
  -> Aff (TestAffE e) (TracedFlow d)
collectData nw period = do
  target /\ cancelers <- liftEff $ do
    target <- newRef []
    cancelers <- do
      let
        onInletData path source d = do
          curData <- readRef target
          _ <- writeRef target $ curData +> InletData path d
          pure unit
        onOutletData path d = do
          curData <- readRef target
          _ <- writeRef target $ curData +> OutletData path d
          pure unit
        -- FIXME: Rpd.subscribeAll should actually subscribe!
        subscribers = R.subscribeAll onInletData onOutletData nw
      performSubs subscribers
    pure $ target /\ cancelers
  delay period
  liftEff $ do
    cancelSubs cancelers
    readRef target


performSubs :: forall e. R.Subscribers e -> Eff (frp :: FRP | e) (Array (R.Canceler e))
performSubs ( outletSubscribers /\ inletSubscribers ) = do
  performSubs'
    $ (fromFoldable $ Map.values outletSubscribers)
      <> (concatMap id $ fromFoldable $ Map.values inletSubscribers)


performSubs' :: forall e. Array (R.Subscriber e) -> Eff (frp :: FRP | e) (Array (R.Canceler e))
performSubs' subscribers =
  traverse performSub subscribers
  where
    performSub sub =
      do
        canceler <- liftEff $ sub
        pure canceler


cancelSubs :: forall e. Array (R.Canceler e) -> Eff (frp :: FRP | e) Unit
cancelSubs cancelers =
  foreachE cancelers $
    \canceler -> do
      _ <- canceler
      pure unit
