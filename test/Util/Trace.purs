module Rpd.Test.Util.Trace
    ( channelsAfter
    , TraceItem(..)
    , (+>)
    ) where

import Prelude

import Data.Array (snoc)
import Data.Time.Duration (Milliseconds)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Covered as Covered

import Effect.Ref (Ref(..))
import Effect.Ref as Ref
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, delay)

import Test.Spec.Assertions (fail)
import Rpd.Test.Util.Actions (getOrFail)

import FSM (run'') as Actions
import Rpd.API.Action (Action(..), DataAction(..))
import Rpd.API.Action.Sequence (ActionList, Sequencer)
import Rpd.Path as P
import Rpd.Toolkit as T
import Rpd.Network as R
import Rpd.Util (Canceler)
import Rpd.Test.Util.Spy as Spy


infixl 6 snoc as +>


data TraceItem d
  = InletData P.ToInlet d
  | OutletData P.ToOutlet d


type TracedFlow d = Array (TraceItem d)


instance showTraceItem :: Show d => Show (TraceItem d) where
    show (InletData iPath d) = show iPath <> " : " <> show d
    show (OutletData oPath d) = show oPath <> " : " <> show d


derive instance eqTraceItem :: Eq d => Eq (TraceItem d)


channelsAfter
  :: forall d c n
   . (Show d)
  => Milliseconds
  -> Sequencer d c n
  -> R.Network d c n
  -> ActionList d c n
  -> Aff (R.Network d c n /\ TracedFlow d)
channelsAfter period sequencer network actions = do
  lastModelSpy <- liftEffect Spy.last
  actionTraceSpy <- liftEffect Spy.trace
  target <- liftEffect $ Ref.new []
  { stop } <- liftEffect $
    Actions.run''
        sequencer
        (pure network)
        (Spy.with lastModelSpy)
        (Spy.with' (handleAction target) actionTraceSpy) -- FIXME: this is the wrong use of spies!
        actions
  maybeNw <- liftEffect $ Spy.get lastModelSpy
  coveredNw <- getOrFail (Covered.fromMaybe "not called" (pure network) maybeNw)
  network' <- getOrFail coveredNw
  delay period
  _ <- liftEffect stop
  vals <- liftEffect $ Ref.read target
  pure $ network' /\ vals
  where
    handleAction target (Data (GotInletData (R.Inlet _ path _ _) d)) = do
        curData <- Ref.read target
        Ref.write (curData +> InletData path d) target
        pure unit
    handleAction target (Data (GotOutletData (R.Outlet _ path _ _) d)) = do
        curData <- Ref.read target
        Ref.write (curData +> OutletData path d) target
        pure unit
    handleAction _ _ = pure unit
