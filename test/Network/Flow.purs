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

import FRP (FRP)
import FRP.Event (create, fold, subscribe) as Event
import FRP.Event.Time (interval)


type CollectedData d =
  (Array (R.InletPath /\ R.DataSource d /\ d)) /\ (Array (R.OutletPath /\ d))


data MyData
  = Bang
  | Str' String String
  | Num' String Int

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
              collectedData `shouldEqual` []
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


collectData :: forall d e. R.Network d -> Milliseconds -> Aff (TestAffE e) (Array R.OutletPath)
collectData nw period = do
  target <- liftEff $ newRef []
  _ <- liftEff $ do
    let
      onInletData path source d = do
        log $ show path -- <> show d
      onOutletData path d = do
        curData <- readRef target
        _ <- writeRef target (path : curData)
        pure unit
      subscribers = R.subscribeAll onInletData onOutletData nw
    performSubs subscribers
  delay period
  liftEff $ readRef target


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
  let
    outletCancelers :: Eff (frp :: FRP | e) (Array (R.Canceler e))
    outletCancelers = traverse performSub $ fromFoldable $ Map.values outletSubscribers
    cancelersTree :: Array (Array (R.Subscriber e))
    cancelersTree = fromFoldable $ Map.values inletSubscribers
    inletCancelers :: Array (Eff (frp :: FRP | e) (Array (R.Canceler e)))
    -- inletCancelers = concatMap performSub $ (fromFoldable $ Map.values inletSubscribers)
    inletCancelers = map (traverse performSub) cancelersTree
    --inletCancelers':: Eff (frp :: FRP | e) (Array (R.Canceler e))
    inletCancelers' :: Eff (frp :: FRP | e) (Array (R.Canceler e))
    inletCancelers' = pure $ concatMap unsafePerformEff inletCancelers
  a <- outletCancelers
  b <- inletCancelers'
  pure $ a <> b
  where
    -- f :: Eff ( frp :: FRP | e ) (Array (Eff ( frp :: FRP | e ) Unit))
    --   -> Eff ( frp :: FRP | e ) (Eff ( frp :: FRP | e ) Unit)
    -- f effects = do
    --   e :: Array (Eff ( frp :: FRP | e ) Unit) <- effects
    --   foreachE e ?what
    performSub sub =
      do
        canceler <- liftEff $ sub
        pure canceler


-- TODO:
-- cancelSubs :: forall e. R.Cancelers e -> Eff (frp :: FRP | e) Unit
-- cancelSubs ( outletSubscribers /\ inletSubscribers ) =
--   foreachE (fromFoldable $ Map.values outletSubscribers) $
--     \sub -> do
--       _ <- liftEff $ sub
--       pure unit
