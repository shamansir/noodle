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

import Effect.Ref (Ref(..))
import Effect.Ref as Ref
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, delay)

import Test.Spec.Assertions (fail)
import Rpd.Test.Util.Either (getOrFail)

import Rpd.API.Action (Action(..), DataAction(..))
import Rpd.API.Action.Sequence (ActionList)
import Rpd.API.Action.Sequence (runTracing) as Actions
import Rpd.Path as P
import Rpd.Toolkit as T
import Rpd.Network as R
import Rpd.Util (Canceler)


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
  -> T.Toolkit d c n
  -> R.Network d c n
  -> ActionList d c n
  -> Aff (R.Network d c n /\ TracedFlow d /\ Canceler)
channelsAfter period toolkit network actions = do
  target <- liftEffect $ Ref.new []
  result /\ { stop } <- liftEffect $
    Actions.runTracing
        toolkit
        network
        (handleAction target)
        actions
  network' <- getOrFail result network
  delay period
  _ <- liftEffect stop
  vals <- liftEffect $ Ref.read target
  pure $ network' /\ vals /\ stop
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
