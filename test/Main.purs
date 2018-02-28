module Test.Main where

import Prelude

import Control.Monad.Aff (Aff, delay, forkAff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as C
import Control.Monad.Eff.Ref (REF)

import Data.Foldable (fold)
import Data.Function (apply, applyFlipped)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration as Duration
import Data.Tuple (Tuple(..))
import Rpd as R
import Signal as S
import Signal.Channel as SC
import Signal.Time (debounce)
import Test.Signal (expect')
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

infixr 0 apply as <|
infixl 1 applyFlipped as |>


data MyNodeType = SumNode | CustomNode
data MyChannelType = NumberChannel | StringChannel

data TestData = AData | BData
data TestErr = AErr | BErr

instance showMyNodeType :: Show (MyNodeType) where
  show SumNode = "SumNode"
  show CustomNode = "CustomNode"


instance showMyChannelType :: Show MyChannelType where
  show NumberChannel = "SumNode"
  show StringChannel = "CustomNode"


instance showTestData :: Show TestData where
  show AData = "AData"
  show BData = "BData"


instance showTestErr :: Show TestErr where
  show AErr = "AErr"
  show BErr = "BErr"


logMessages
  :: forall e n c a x
   . Show n => Show c
  => R.Messages n c a x
  -> Eff ( console :: C.CONSOLE | e ) Unit
logMessages sig = do
  S.runSignal (sig S.~> (\msg -> C.log (show msg)))


getShowSignal
  :: forall n c a x
   . Show n => Show c -- => Show a => Show x
  => R.Messages n c a x
  -> S.Signal String
getShowSignal sig =
  sig S.~> show


main :: forall eff. Eff (RunnerEffects ( ref :: REF, channel :: SC.CHANNEL | eff )) Unit
main = run [consoleReporter] do
  describe "Rpd" do
    describe "Running the application" do
      it "runs in empty configuration" do
        _ <- liftEff (R.run [] R.network)
        pure unit
      it "able to log messages" do
        let network = R.network
        app <- liftEff $ R.run [] network
        let
          messages :: R.Messages MyNodeType MyChannelType TestData TestErr
          --messages = S.constant R.Start
          messages = R.getMessages app
        liftEff (logMessages messages)
        pure unit
      it "fires expected messages on creation" do
        let
          -- network :: forall e a x. R.NetworkActions e MyNodeType MyChannelType TestData TestErr
          network = R.network
        app <- liftEff $ R.run [] network
        let
          messages :: R.Messages MyNodeType MyChannelType TestData TestErr
          messages = R.getMessages app
          showSig = messages |> getShowSignal
        expect' showSig ["Start"]
        pure unit
      it "creates the complex network" do
        app <- liftEff (R.run [] do
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
        let
          messages :: R.Messages MyNodeType MyChannelType TestData TestErr
          messages = R.getMessages app
          showSig = messages |> getShowSignal
        expect' showSig ["Start"] -- ["Start, "]
        pure unit


-- expectMessages
--   :: forall e n c a x
--    . Show n => Show c => Show a => Show x
--   => Array String
--   -> R.App n c a x e
--   -> Aff (ref :: REF, channel :: SC.CHANNEL, avar :: AVAR | e) Unit
-- expectMessages expectation app = do
--   let
--     -- messages :: R.Messages MyNodeType MyChannelType TestData TestErr
--     messages = R.getMessages app
--     showSig = messages |> getShowSignal
--   expect' showSig expectation -- ["Start"]

wait :: forall e. Number -> Aff e Unit
wait t = do
  delay (Duration.Milliseconds t)
  pure unit
