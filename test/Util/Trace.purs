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
import Data.Array as Array

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
  lastModelSpy <- liftEffect $ Spy.last' $ pure network
  actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap handleAction
  { stop } <- liftEffect $
    Actions.run''
        sequencer
        (pure network)
        (Spy.with lastModelSpy)
        (Spy.with actionTraceSpy)
        actions
  {-
  maybeNw <- liftEffect $ Spy.get lastModelSpy
  coveredNw <- getOrFail (Covered.fromMaybe "not called" (pure network) maybeNw)
  -}
  coveredNw <- liftEffect $ Spy.get lastModelSpy
  network' <- getOrFail coveredNw
  delay period
  _ <- liftEffect stop
  vals <- liftEffect $ Spy.get actionTraceSpy
  pure $ network' /\ Array.catMaybes vals
  where
    handleAction (Data (GotInletData (R.Inlet _ path _ _) d)) = Just $ InletData path d
    handleAction (Data (GotOutletData (R.Outlet _ path _ _) d)) = Just $ OutletData path d
    handleAction _ = Nothing
