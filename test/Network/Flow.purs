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
import Rpd.Flow
  ( flow
  , subscribeAll, subscribeTop
  , Subscribers, Subscriber, Canceler
  ) as R

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
                [ R.inlet' "mouth" $ R.flow $ const Pills <$> interval 50 ]
                [ ]
            ]
          ]

      runWith network
        \nw ->
          do
            collectedData <- collectData nw (Milliseconds 300.0)
            collectedData `shouldEqual` (replicate 5 $ InletData (inletPath 0 0 0) Pills)
            pure unit

    it "we're able to collect the data from the flow attached to the outlet" $ do

      let
        network =
          R.network [
            R.patch "Test 0001" [
              R.node "Producer"
                [ ]
                [ R.outlet' "factory" $ R.flow $ const Banana <$> interval 50 ]
            ]
          ]

      runWith network
        \nw ->
          do
            collectedData <- collectData nw (Milliseconds 300.0)
            collectedData `shouldEqual`
              (replicate 5 $ OutletData (outletPath 0 0 0) Banana)
            pure unit

    pending' "connecting the outlet to the inlet actually sends the data" $ do
      -- TODO: test it for the "after" case below first, then bring back

      let
        factory = R.outlet' "factory"
                    $ R.flow $ const Banana <$> interval 50
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
        patch' = R.connect (outletPath 0 0 0) (inletPath 0 1 0) patch
        network = R.network [ patch' ]

      runWith network
        \nw ->
          do
            collectedData <- collectData nw (Milliseconds 300.0)
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
            collectedData <- collectData nw (Milliseconds 510.0)
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
              , OutletData (outletPath 0 0 1) Letter
              ]
            pure unit

  describe "flow is defined after running the system" do

    it "connecting the outlet to the inlet actually sends the data" $ do
      let
        factory = R.outlet' "factory" $ R.flow $ const Banana <$> interval 50
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
        network = R.network [ patch ]

      runWith network
        \nw ->
          do
            collectedData <- collectData nw (Milliseconds 300.0)
            collectedData `shouldEqual`
                (replicate 5 $ OutletData (outletPath 0 0 0) Banana)

            let nw' = fromMaybe nw $ R.connect' (outletPath 0 0 0) (inletPath 0 1 0) nw
            collectedInletData <-
              collectTopDataFromInlet nw' (inletPath 0 1 0) (Milliseconds 300.0)
            collectedInletData `shouldEqual`
                (replicate 5 $ Just (outletPath 0 0 0) /\ Banana)

            pure unit

    it "connecting the outlet to the inlet and then disconnecting it works as well" $ do

      let
        factory1 = R.outlet' "factory-1" $ R.flow $ const Banana <$> interval 60
        factory2 = R.outlet' "factory-2" $ R.flow $ const Liver <$> interval 50
        consume = R.inlet "consumer"
        producerNode =
          R.node "Producer"
            [ ]
            [ factory1, factory2 ]
        receiverNode =
          R.node "Receiver"
            [ consume ]
            [ ]
        patch =
          R.patch "Test 0001"
            [ producerNode
            , receiverNode
            ]
        network = R.network [ patch ]

      runWith network
        \nw ->
          do

            collectedData <- collectData nw (Milliseconds 300.0)
            collectedData `shouldEqual`
                ((concat $ replicate 4 $
                  [ OutletData (outletPath 0 0 1) Liver
                  , OutletData (outletPath 0 0 0) Banana
                  ]
                ) +> OutletData (outletPath 0 0 1) Liver)

            let nw' = fromMaybe nw $ R.connect' (outletPath 0 0 0) (inletPath 0 1 0) nw
            collectedInletData <-
              collectTopDataFromInlet nw' (inletPath 0 1 0) (Milliseconds 300.0)
            collectedInletData `shouldEqual`
                (replicate 4 $ Just (outletPath 0 0 0) /\ Banana)

            let nw'' = fromMaybe nw' $ R.connect' (outletPath 0 0 1) (inletPath 0 1 0) nw'
            collectedInletData' <-
              collectTopDataFromInlet nw'' (inletPath 0 1 0) (Milliseconds 300.0)
            collectedInletData' `shouldEqual`
                (replicate 5 $ Just (outletPath 0 0 1) /\ Liver)

            let nw''' = fromMaybe nw'' $ R.disconnectTop (inletPath 0 1 0) nw''
            collectedInletData'' <-
              collectTopDataFromInlet nw''' (inletPath 0 1 0) (Milliseconds 300.0)
            collectedInletData'' `shouldEqual`
                (replicate 4 $ Just (outletPath 0 0 0) /\ Banana)

            pure unit

    it "connecting several outlets to the inlet merges their flows" $ do
      let
        factory1 = R.outlet' "factory-1" $ R.flow $ const Banana <$> interval 60
        factory2 = R.outlet' "factory-2" $ R.flow $ const Liver <$> interval 50
        consume = R.inlet "consumer"
        producerNode1 =
          R.node "Producer-1"
            [ ]
            [ factory1 ]
        producerNode2 =
          R.node "Producer-2"
            [ ]
            [ factory2 ]
        receiverNode =
          R.node "Receiver"
            [ consume ]
            [ ]
        patch =
          R.patch "Test 0001"
            [ producerNode1
            , producerNode2
            , receiverNode
            ]
        network = R.network [ patch ]

      runWith network
        \nw ->
          do
            let nw' = fromMaybe nw $ R.connect' (outletPath 0 0 0) (inletPath 0 2 0) nw
            let nw'' = fromMaybe nw' $ R.connect' (outletPath 0 1 0) (inletPath 0 2 0) nw'
            collectedData <- collectData nw'' (Milliseconds 300.0)
            collectedData `shouldEqual`
                ((concat $ replicate 4 $
                  [ OutletData (outletPath 0 1 0) Liver
                  , InletData  (inletPath  0 2 0) Liver
                  , OutletData (outletPath 0 0 0) Banana
                  , InletData  (inletPath  0 2 0) Banana
                  ]
                ) +> OutletData (outletPath 0 1 0) Liver
                  +> InletData (inletPath  0 2 0) Liver)
            pure unit

    it "disconnecting all the sources stops the flow" $ do

      let
        factory1 = R.outlet' "factory-1" $ R.flow $ const Banana <$> interval 60
        factory2 = R.outlet' "factory-2" $ R.flow $ const Liver <$> interval 50
        consume = R.inlet "consumer"
        producerNode =
          R.node "Producer"
            [ ]
            [ factory1
            , factory2
            ]
        receiverNode =
          R.node "Receiver"
            [ consume ]
            [ ]
        patch =
          R.patch "Test 0001"
            [ producerNode
            , receiverNode
            ]
        network = R.network [ patch ]

      runWith network
        \nw ->
          do
            let
              nw' = fromMaybe nw $ R.connect' (outletPath 0 0 0) (inletPath 0 2 0) nw
              nw'' = fromMaybe nw' $ R.connect' (outletPath 0 1 0) (inletPath 0 2 0) nw'
              nw''' = fromMaybe nw'' $ R.disconnectTop (inletPath 0 1 0) nw''
              nw'''' = fromMaybe nw''' $ R.disconnectTop (inletPath 0 1 0) nw'''
            collectedData <- collectTopDataFromInlet nw'''' (inletPath 0 1 0) (Milliseconds 300.0)
            collectedData `shouldEqual` []
            collectedData' <- collectData nw'''' (Milliseconds 300.0)
            collectedData' `shouldEqual`
              ((concat $ replicate 4 $
                [ OutletData (outletPath 0 0 1) Liver
                , OutletData (outletPath 0 0 0) Banana
                ]) +> OutletData (outletPath 0 0 1) Liver)
            pure unit

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
    , R.inlet' "Buyer UG" $ R.flow $ const Pills <$> interval 2500
    , R.inlet "Mrs. Black Widow"
    , R.inlet "eBay"
    , R.inlet' "kleinanzeige"
        $ R.flow $ map Curse $ Event.fold (\_ n -> n + 1) (interval 100) 0
    ]
    [ R.outlet "Postman 1"
    , R.outlet' "Postman 2" $ R.flow
        $ const Letter <$> (interval 250)
    , R.outlet' "Truck 1" $ R.flow
        $ map Apple
        $ Event.fold (\_ n -> n + 1) (interval 100) 0
    ]


upsNode :: R.LazyNode Delivery
upsNode =
  R.node "UPS"
    [ R.inlet "Incoming 1"
    , R.inlet "Incoming 2"
    ]
    [ R.outlet "Outgoing 1"
    , R.outlet' "Outgoing 2"
        $ R.flow $ const Banana <$> interval 50
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


data TraceItem d
  = InletData R.InletPath d
  | OutletData R.OutletPath d

type TracedFlow d = Array (TraceItem d)

type TracedInletFlow d = Array (Maybe R.OutletPath /\ d)

derive instance genericTraceItem :: Generic (TraceItem d) _

instance showTraceItem :: Show d => Show (TraceItem d) where
  show = genericShow

instance eqTraceItem :: Eq d => Eq (TraceItem d) where
  eq = genericEq


andExpectToReceiveFromNetwork
  :: forall e d
   . Show d => Eq d
  => (R.Network d -> Maybe (R.Network d))
  -> TracedFlow d
  -> Milliseconds
  -> R.Network d
  -> Aff (TestAffE e) (R.Network d)
andExpectToReceiveFromNetwork f expectedData delay nw = do
  let nw' = fromMaybe nw $ f nw
  collectedData <- collectData nw delay
  collectedData `shouldEqual` expectedData
  pure nw'


andExpectToReceiveFromInlet
  :: forall e d
   . Show d => Eq d
  => (R.Network d -> Maybe (R.Network d))
  -> R.InletPath
  -> Milliseconds
  -> TracedInletFlow d
  -> R.Network d
  -> Aff (TestAffE e) (R.Network d)
andExpectToReceiveFromInlet f inletPath delay expectedData nw = do
  let nw' = fromMaybe nw $ f nw
  collectedData <- collectTopDataFromInlet nw inletPath delay
  collectedData `shouldEqual` expectedData
  pure nw'


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
        subscribers = R.subscribeAll onInletData onOutletData nw
      performSubs subscribers
    pure $ target /\ cancelers
  delay period
  liftEff $ do
    cancelSubs cancelers
    readRef target


collectTopDataFromInlet
  :: forall d e
   . (Show d)
  => R.Network d
  -> R.InletPath
  -> Milliseconds
  -> Aff (TestAffE e) (TracedInletFlow d)
collectTopDataFromInlet nw inletPath period = do
  target /\ canceler <- liftEff $ do
    target <- newRef []
    canceler <- do
      let
        extractOutletSrc source =
          case source of
            R.UserSource _ -> Nothing
            R.OutletSource outletPath _ -> Just outletPath
        onInletData source d = do
          curData <- readRef target
          _ <- writeRef target $ curData +> (extractOutletSrc source /\ d)
          pure unit
        subscriber = fromMaybe (pure $ pure unit)
          $ R.subscribeTop onInletData inletPath nw
      performSub subscriber
    pure $ target /\ canceler
  delay period
  liftEff $ do
    cancelSub canceler
    readRef target


performSub :: forall e. R.Subscriber e -> Eff (frp :: FRP | e) (R.Canceler e)
performSub sub =
  do
    canceler <- liftEff $ sub
    pure canceler


cancelSub :: forall e. R.Canceler e -> Eff (frp :: FRP | e) Unit
cancelSub canceler = do
  _ <- canceler
  pure unit


performSubs :: forall e. R.Subscribers e -> Eff (frp :: FRP | e) (Array (R.Canceler e))
performSubs ( outletSubscribers /\ inletSubscribers ) = do
  performSubs'
    $ (fromFoldable $ Map.values outletSubscribers)
      <> (concatMap id $ fromFoldable $ Map.values inletSubscribers)


performSubs' :: forall e. Array (R.Subscriber e) -> Eff (frp :: FRP | e) (Array (R.Canceler e))
performSubs' subscribers =
  traverse performSub subscribers


cancelSubs :: forall e. Array (R.Canceler e) -> Eff (frp :: FRP | e) Unit
cancelSubs cancelers =
  foreachE cancelers cancelSub
