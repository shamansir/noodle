module Noodle.Test.Spec.Flow.Network
    ( spec
    ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))

import Effect.Class (liftEffect)
import Effect.Aff (delay)

import Test.Spec (Spec, it, pending)
import Test.Spec.Assertions (shouldEqual)

import FSM (run_) as Actions

import Noodle.Network as Network

import Noodle.Test.Util.Spy as Spy
import Noodle.Test.Util.Trace (collectData)
import Noodle.Test.Spec.Flow.Base (mySequencer)


{- ======================================= -}
{- =============== NETWORK =============== -}
{- ======================================= -}


spec :: Spec Unit
spec = do
  it "we receive no data from the network when it's empty" $ do
    actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap collectData

    _ <- liftEffect $ Actions.run_
        mySequencer
        (pure $ Network.empty "foo")
        (Spy.with actionTraceSpy)

    delay $ Milliseconds 100.0

    collectedData <- liftEffect $ Spy.get actionTraceSpy
    collectedData `shouldEqual` []

  pending "all the cancelers are called after running the system"

