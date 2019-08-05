module RpdTest.Helper
    ( withRpd
    , withRpd_
    ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_)

import Test.Spec.Assertions (fail)

import Rpd.API.Action.Sequence (ActionList, ErrorHandler(..), EveryStep, LastStep)
import Rpd.API.Action.Sequence (run, run_) as Actions
import Rpd.Toolkit as T
import Rpd.Network as R


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
