module Test.Fn where

import Prelude

import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

import Control.Monad.State (modify_)
import Control.Monad.Error.Class (class MonadThrow)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Exception (Error)
import Effect.Aff (Aff, throwError, error)

import Test.Spec (Spec, pending, describe, it, pending')
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Signal (expectFn, expect)

-- import Noodle.Node.Shape (noInlets, noOutlets) as Shape
-- import Noodle.Node ((<~>), (+>), (<+))
import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Fn (Fn, Fn')
import Noodle.Fn as Fn
import Noodle.Fn.Process as Fn
import Noodle.Fn.Protocol (Protocol)

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


type Tracker d = Ref (String /-> d)


newTracker :: forall d. Effect (Tracker d)
newTracker = Ref.new Map.empty


shouldContain :: forall d. Eq d => Show d => String -> d -> Tracker d -> Aff Unit
shouldContain id val tracker = do
    values <- liftEffect $ Ref.read tracker
    case Map.lookup id values of
        Just otherVal ->
            if val == otherVal then pure unit
            else throwError $ error $ show val <> " /= " <> show otherVal
        Nothing ->
            throwError $ error $ "\"" <> id <> "\" was not found in tracker"


makeProtocol :: forall d. Array (String /\ d) -> Tracker d -> Tracker d -> Effect (Protocol String String d)
makeProtocol initialAtInputs inputs outputs =
    { last : Nothing
    , receive : pure <<< const Nothing
    , send : const $ const $ pure unit
    , sendIn : const $ const $ pure unit
    } # pure


spec :: Spec Unit
spec = do

    describe "foo" $ do

        it "summing works" $ do
            inputsTracker :: Tracker Int <- liftEffect newTracker
            outputsTracker :: Tracker Int <- liftEffect newTracker
            let
                fn :: forall m. MonadEffect m => Fn String String m Int
                fn =
                    Fn.make "foo" [ "a", "b" ] [ "sum" ] $ do
                        a <- Fn.receive "a"
                        b <- Fn.receive "b"
                        Fn.send "sum" $ a + b
            testProtocol <- liftEffect
                                $ makeProtocol [ "a" /\ 0, "b" /\ 0 ]
                                    inputsTracker outputsTracker
            Fn.run 0 testProtocol fn
            outputsTracker # shouldContain "sum" 8
            pure unit

        it "summing works with sendIn" $ do
            inputsTracker :: Tracker Int <- liftEffect $ newTracker
            outputsTracker :: Tracker Int <- liftEffect $ newTracker
            let
                fn :: forall m. MonadEffect m => Fn String String m Int
                fn =
                    Fn.make "foo" [ "a", "b" ] [ "sum" ] $ do
                        Fn.sendIn "a" 6
                        Fn.sendIn "b" 7
                        a <- Fn.receive "a"
                        b <- Fn.receive "b"
                        Fn.send "sum" $ a + b
            testProtocol <- liftEffect
                                $ makeProtocol [ "a" /\ 0, "b" /\ 0 ]
                                    inputsTracker outputsTracker
            Fn.run 0 testProtocol fn
            outputsTracker # shouldContain "sum" 13
            inputsTracker # shouldContain "a" 6
            inputsTracker # shouldContain "b" 7
            pure unit

    describe "bar" $ do
        pure unit
