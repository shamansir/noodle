module Test.Main where

import Prelude

import Data.Function (apply, applyFlipped)
import Data.Time.Duration as Duration
import Data.Tuple (Tuple(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)

import Control.Monad.Aff (Aff, delay, forkAff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console as C

import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import Signal as S
import Signal.Channel as SC
import Signal.Time (debounce)

import Test.Signal (expect')

import Rpd as R

infixr 0 apply as <|
infixl 1 applyFlipped as |>

data MyNodeType = SumNode | CustomNode
data MyChannelType = NumberChannel | StringChannel

main :: forall eff. Eff (RunnerEffects ( ref :: REF, channel :: SC.CHANNEL | eff )) Unit
main = run [consoleReporter] do
  describe "Rpd" do
    describe "Running the application" do
      it "runs in empty configuration" do
        _ <- liftEff (R.run [] R.network)
        pure unit
      it "able to log messages" do
        app <- liftEff (R.run [] R.network)
        let messages = R.getMessages app
        -- _ <- forkAff $ expect' ?what []
        -- liftEff (S.runSignal messages)
        -- liftEff (S.runSignal (messages S.~> (\msg -> show msg) S.~> C.log))
        -- S.runSignal (map show messages S.~> C.log)
        pure unit
      it "creates the complex network" do

        -- app <- R.run [] R.network
        -- messages <- R.getMessages app
        -- _ <- forkAff $ expect' ?what []
        -- liftEff (S.runSignal messages)
        -- S.runSignal (map show messages S.~> C.log)
        (R.App app) <- liftEff (R.run [] do
            let
              toInt = (\_ -> 20)
              myPatch = R.patch "MyPatch"
              sumNode = R.node SumNode "sum"
              inletA = R.getInlet "a" sumNode
              inletB = R.getInlet "b" sumNode
              myCustomNode =
                R.node CustomNode "Custom"
                  |> R.addInlet (R.inlet NumberChannel "a" |> R.allow
                      [ Tuple StringChannel toInt ])
                  |> R.addInlet (R.inlet NumberChannel "b" |> R.default 10)
                  |> R.addOutlet (R.outlet NumberChannel "out")
                  -- |> R.process (\inlets -> { out: inlets.a * inlets.b })
                  |> R.process
                    (\inlets ->
                      Map.empty
                      -- Map.singleton "out"
                      --   ((Map.lookup "a" inlets |> fromMaybe (R.Data 0)) *
                      --    (Map.lookup "b" inlets |> fromMaybe (R.Data 0)))
                    )
            -- inletA |> R.send 10 |> R.send 20
            -- inletB |> R.send 10 |> R.send 10 |> R.send 5
            -- -- inletB |> R.send "10" |> R.send "10" |> R.send "5"
            -- myCustomNode |> R.getInlet "a" |> R.send 12 |> R.send 11
            -- myCustomNode |> R.getOutlet "out" |> R.connect (sumNode |> R.getInlet "a")
            -- myPatch |> R.addNode sumNode
            -- myCustomNode |> R.getInlet "b" |> R.send 13
            let myNetwork = R.network |> R.addPatch myPatch
            myNetwork)
        pure unit


wait :: forall e. Number -> Aff e Unit
wait t = do
  delay (Duration.Milliseconds t)
  pure unit
