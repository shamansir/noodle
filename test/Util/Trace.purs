module Noodle.Test.Util.Trace
    ( TraceItem(..)
    , (+>)
    , collectData
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
import Noodle.Test.Util.Actions (getOrFail)

import FSM (run'', pushAll) as Actions
import Noodle.API.Action (Action(..), DataAction(..))
import Noodle.API.Action.Sequence (ActionList, Sequencer)
import Noodle.Path as P
import Noodle.Toolkit as T
import Noodle.Network as R
import Noodle.Util (Canceler)
import Noodle.Test.Util.Spy as Spy


infixl 6 snoc as +>


data TraceItem d
  = InletData P.ToInlet d
  | OutletData P.ToOutlet d


type TracedFlow d = Array (TraceItem d)


instance showTraceItem :: Show d => Show (TraceItem d) where
    show (InletData iPath d) = show iPath <> " : " <> show d
    show (OutletData oPath d) = show oPath <> " : " <> show d


derive instance eqTraceItem :: Eq d => Eq (TraceItem d)


collectData (Data (GotInletData (R.Inlet _ path _ _) d)) = Just $ InletData path d
collectData (Data (GotOutletData (R.Outlet _ path _ _) d)) = Just $ OutletData path d
collectData _ = Nothing



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
  actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap collectData
  { push, stop } <- liftEffect $
    Actions.run''
        sequencer
        (pure network)
        (Spy.with lastModelSpy)
        (Spy.with actionTraceSpy)
  _ <- liftEffect $ Actions.pushAll push actions
  coveredNw <- liftEffect $ Spy.get lastModelSpy
  network' <- getOrFail coveredNw
  delay period
  _ <- liftEffect stop
  vals <- liftEffect $ Spy.get actionTraceSpy
  pure $ network' /\ Array.catMaybes vals
