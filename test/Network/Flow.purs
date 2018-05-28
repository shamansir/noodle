module RpdTest.Network.Flow
    ( spec ) where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Util (TestAffE, runWith)

import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.Eff.Console (log, CONSOLE)

import Rpd as R
import Rpd.Flow (flow, subscribeAll, Subscribers, Cancelers, Subscriber, Canceler) as R

import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array ((:), fromFoldable, concatMap, fold, intercalate)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)

import FRP (FRP)
import FRP.Event (create, fold, subscribe) as Event
import FRP.Event.Time (interval)


type CollectedData d =
  (Array (R.InletPath /\ R.DataSource d /\ d)) /\ (Array (R.OutletPath /\ d))


data MyData
  = Bang
  | Str' String String
  | Num' String Int

derive instance genericMyData :: Generic MyData _

instance showMyData :: Show MyData where
  show = genericShow

instance eqMyData :: Eq MyData where
  eq = genericEq


node :: String -> R.LazyNode MyData
node nodeId =
  R.node "f"
    [ R.inlet "a" -- WithDefault "a" (Str' (nodeId <> "a") "i")
    , R.inletWithDefault "b" $ Str' (nodeId <> "b") "test"
    , R.inlet' "f" $ R.flow $ map (Num' (nodeId <> "f")) $ interval 5000
    , R.inlet "d" -- (ST.every ST.second S.~> Num' (nodeId <> "d"))
    , R.inlet "e" -- WithDefault "e" (Num' (nodeId <> "e") 3.0)
    ]
    [ R.outlet "c"
    , R.outlet' "x" $ R.flow
        $ map (Num' (nodeId <> "x"))
        $ Event.fold (\_ n -> n + 1) (interval 500) 0
    , R.outlet' "y" $ R.flow
        $ map (Num' (nodeId <> "y"))
        $ Event.fold (\_ n -> n + 1) (interval 200) 0
    ]
    -- (\_ -> [ "c" /\ Int' 10 ] )

network :: R.Network MyData
network =
  R.network
    [ R.patch "Patch One"
      [ node "1"
      , R.processWith processF $ node "2"
      ] -- >>> connect (patch.getNode 0) "a" (patch.getNode 1) "b"
    ]
  where
    processF inputs | Map.isEmpty inputs = Map.empty
    processF inputs | Map.member "d" inputs =
      Map.singleton "c" $ fromMaybe Bang $ Map.lookup "d" inputs
    processF inputs = Map.empty

spec :: forall e. Spec (TestAffE e) Unit
spec = do
  describe "subscribing to the data flow" do
      it "receives the data from events" do
        -- TODO: move tests for a network in the module with this network, export as a suite
        runWith network
          \nw ->
            do
              collectedData <- collectData network (Milliseconds 1000.0)
              liftEff $ log $ "collected: " <> show collectedData
              collectedData `shouldEqual`
                [ outletPath 0 1 2 /\ Num' "2y" 4
                , outletPath 0 0 2 /\ Num' "1y" 4
                , outletPath 0 1 2 /\ Num' "2y" 3
                , outletPath 0 0 2 /\ Num' "1y" 3
                , outletPath 0 1 1 /\ Num' "2x" 1
                , outletPath 0 0 1 /\ Num' "1x" 1
                , outletPath 0 1 2 /\ Num' "2y" 2
                , outletPath 0 0 2 /\ Num' "1y" 2
                , outletPath 0 1 2 /\ Num' "2y" 1
                , outletPath 0 0 2 /\ Num' "1y" 1
                ]
              pure unit
  describe "connecting channels after creation" do
    pure unit
  describe "disconnecting channels after creation" do
    pure unit
  describe "manually sending data to the channels after creation" do
    pure unit
  describe "manually sending delayed data to the channels after creation" do
    --   delay (Milliseconds 100.0)
    pure unit
  describe "adding nodes after creation" do
    pure unit
  describe "deleting nodes after creation" do
    pure unit
  where
    outletPath :: Int -> Int -> Int -> R.OutletPath
    outletPath a b c = R.OutletPath (R.NodePath (R.PatchId a) b) c


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


collectData
  :: forall e
   . R.Network MyData
  -> Milliseconds
  -> Aff (TestAffE e) (Array (R.OutletPath /\ MyData))
collectData nw period = do
  target /\ cancelers <- liftEff $ do
    target <- newRef []
    cancelers <- do
      let
        onInletData path source d = do
          log $ show path <> show d
        onOutletData path d = do
          curData <- readRef target
          _ <- writeRef target $ (path /\ d) : curData
          pure unit
        subscribers = R.subscribeAll onInletData onOutletData nw
      performSubs subscribers
    pure $ target /\ cancelers
  delay period
  liftEff $ do
    cancelSubs cancelers
    readRef target


-- expectFn :: forall e a. Eq a => Show a => Signal a -> Array a -> Test (ref :: REF | e)
-- expectFn sig vals = makeAff \resolve -> do
--   remaining <- newRef vals
--   let getNext val = do
--         nextValArray <- readRef remaining
--         let nextVals = fromFoldable nextValArray
--         case nextVals of
--           Cons x xs -> do
--             if x /= val then resolve $ Left $ error $ "expected " <> show x <> " but got " <> show val
--               else case xs of
--                 Nil -> resolve $ Right unit
--                 _ -> writeRef remaining (toUnfoldable xs)
--           Nil -> resolve $ Left $ error "unexpected emptiness"
--   runSignal $ sig ~> getNext
--   pure nonCanceler

-- expect :: forall e a. Eq a => Show a => Int -> Signal a -> Array a -> Test (ref :: REF, timer :: TIMER, avar :: AVAR | e)
-- expect time sig vals = timeout time $ expectFn sig vals


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
