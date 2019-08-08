module RpdTest.Helper
    ( withRpd
    , withRpd'
    , withRpd_
    , channelsAfter
    , TraceItem(..)
    ) where

import Prelude

import Data.Array (snoc)
import Data.Time.Duration (Milliseconds)

import Effect.Ref as Ref
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_, delay)

import Test.Spec.Assertions (fail)

import Rpd.API.Action (Action(..), DataAction(..))
import Rpd.API.Action.Sequence (ActionList, ErrorHandler(..), EveryAction(..), EveryStep(..), LastStep)
import Rpd.API.Action.Sequence (run, run_, runTracing) as Actions
import Rpd.Path as P
import Rpd.Toolkit as T
import Rpd.Network as R


infixl 6 snoc as +>


failOnError :: ErrorHandler
failOnError = ErrorHandler $ launchAff_ <<< fail <<< show


withRpd
    :: forall d c n
     . T.Toolkit d c n
    -> R.Network d c n
    -> ActionList d c n
    -> EveryStep d c n
    -> Aff Unit
withRpd toolkit network actions everyStep =
  liftEffect $
     Actions.run
        toolkit
        network
        failOnError
        everyStep
        actions


withRpd'
    :: forall d c n
     . T.Toolkit d c n
    -> R.Network d c n
    -> ActionList d c n
    -> Aff Unit
withRpd' toolkit network actions =
  liftEffect $
     Actions.run
        toolkit
        network
        failOnError
        (EveryStep $ const $ pure unit)
        actions


withRpd_
    :: forall d c n
     . T.Toolkit d c n
    -> R.Network d c n
    -> ActionList d c n
    -> LastStep d c n
    -> Aff Unit
withRpd_ toolkit network actions lastStep =
  liftEffect $
     Actions.run_
        toolkit
        network
        failOnError
        lastStep
        actions


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
  -> Aff (TracedFlow d)
channelsAfter period toolkit network actions = do
  target <- liftEffect $ Ref.new []
  _ <- liftEffect $
    Actions.runTracing
        toolkit
        network
        failOnError
        (EveryAction $ handleAction target)
        actions
  delay period
  vals <- liftEffect $ Ref.read target
  pure vals
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

