module Test.Protocol2 where

import Prelude

import Data.Map as Map
import Data.Array as Array
import Data.Map.Extra (type (/->))
import Data.Maybe (Maybe(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Symbol (reflectSymbol, class IsSymbol)
import Prim.Symbol (class Compare)
import Record.Extra (keys)
import Effect.Console (log) as Console
import Type.Proxy (Proxy(..))

import Control.Monad.State (modify_, get) as State
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
-- import Noodle.Fn (Fn, Fn')
-- import Noodle.Fn as Fn
-- import Noodle.Fn.Process as Fn
-- import Noodle.Fn.Protocol (Protocol)
-- import Noodle.Fn.Protocol as Protocol

import Noodle.Fn2.Process (ProcessM)
import Noodle.Fn2.Process as Fn
import Noodle.Fn2.Process as Process
import Noodle.Fn2.Protocol (Protocol)
import Noodle.Fn2.Protocol as Protocol
import Noodle.Id (reflect')
import Noodle.Id (Family(..)) as Node
import Noodle.Id (Input(..), Output(..)) as Fn

import Unsafe.Coerce (unsafeCoerce)

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT

import Type.Data.Symbol (compare)

import Record.Extra as Record


type TestInputs = ( foo :: String, i2 :: Boolean )
type TestOutputs = ( bar :: Int, o2 :: Boolean )


_fooInput = Fn.Input 1 :: Fn.Input "foo"
_i3Input = Fn.Input 3 :: Fn.Input "i3"
_barOutput = Fn.Output 1 :: Fn.Output "bar"



testSend ∷ ∀ state is m. Int → ProcessM state is TestOutputs m Unit
testSend int = Fn.send _barOutput int


testSendIn ∷ ∀ state os m. String → ProcessM state TestInputs os m Unit
testSendIn str = Fn.sendIn _fooInput str


-- testSendIn' ∷ ∀ state os m. String → ProcessM state TestInputs os m Unit
-- testSendIn' str = Fn.sendIn _i3Input 12


testReceive ∷ ∀ state os m. ProcessM state TestInputs os m String
testReceive = Fn.receive _fooInput


spec :: Spec Unit
spec =
    do
        let initialInputs = { foo : "foo", i2 : false, i3 : 14 }
        let initialOutputs = { bar : 4, o2 : true }
        let initialState = 2
        let makeProtocol = Protocol.make initialState initialInputs initialOutputs

        describe "keys" $ do

            it "we can get keys of the record" $ do
                (Array.toUnfoldable [ "foo", "i2", "i3" ]) `shouldEqual` Fn.inputsOf initialInputs

            it "we can get keys of the inputs" $ do
                tracker /\ protocol <- makeProtocol
                inputs <- liftEffect $ Protocol.inputs tracker
                (Array.toUnfoldable [ "foo", "i2", "i3" ]) `shouldEqual` Fn.inputsOf inputs

            it "we can get keys of the typed input" $ do
                (Array.toUnfoldable [ "foo", "i2" ]) `shouldEqual` (Record.keys (Proxy :: _ TestInputs))

        describe "protocol" $ do

            it "values stay initial until changed" $ do
                tracker /\ protocol <- makeProtocol
                curState <- liftEffect $ Signal.get tracker.state
                curState `shouldEqual` 2
                atFoo <- liftEffect $ Protocol.inputs tracker <#> _.foo
                atFoo `shouldEqual` "foo"
                atBar <- liftEffect $ Signal.get tracker.outputs <#> Tuple.snd <#> _.bar
                atBar `shouldEqual` 4
                pure unit

        describe "process" $ do

            it "sending to input changes its value in refs" $ do
                tracker /\ protocol <- makeProtocol
                _ <- Process.runM protocol $ Fn.sendIn _fooInput "foobar"
                atFoo <- liftEffect $ Protocol.inputs tracker <#> _.foo
                atFoo `shouldEqual` "foobar"
                pure unit

            it "sending to input two times its value in refs" $ do
                tracker /\ protocol <- makeProtocol
                _ <- Process.runM protocol $ Fn.sendIn _fooInput "foobar"
                _ <- Process.runM protocol $ Fn.sendIn _fooInput "barfoo"
                atFoo <- liftEffect $ Protocol.inputs tracker <#> _.foo
                atFoo `shouldEqual` "barfoo"
                pure unit

            it "sending to input updates last input ref" $ do
                tracker /\ protocol <- makeProtocol
                _ <- Process.runM protocol $ Fn.sendIn _fooInput "foobar"
                lastInput <- liftEffect $ Protocol.lastInput tracker
                case lastInput of
                    Just input ->
                        -- let (str :: String) = unsafeCoerce input
                        -- in
                        -- liftEffect $ Console.log str
                        -- pure unit
                        -- Fn.unsafeInputToString (unsafeCoerce input) `shouldEqual` "foo"
                        -- Fn.inputToString input `shouldEqual` "foo"

                        -- pure unit
                        (reflect' input) `shouldEqual` "foo"
                    Nothing -> fail "no last input was recorded"
                pure unit

            it "sending to input updates twice last input ref anyway" $ do
                tracker /\ protocol <- makeProtocol
                _ <- Process.runM protocol $ Fn.sendIn _fooInput "foobar"
                _ <- Process.runM protocol $ Fn.sendIn _i3Input 12
                -- (lastInput :: Maybe (forall x. IsSymbol x => Fn.Input x)) <- liftEffect $ Signal.get protocolS.lastInput
                lastInput <- liftEffect $ Protocol.lastInput tracker
                case lastInput of
                    Just input ->
                        -- liftEffect $ Console.log (reflectSymbol (unsafeCoerce (Fn.iToSProxy (unsafeCoerce input))))
                        -- liftEffect $ Console.log (Fn.iToSProxy (unsafeCoerce input))
                        -- let (cmp :: Proxy _) = (cmp :: Compare (SProxy "foo") _)
                        -- in Console.log (unsefeCoerce cmp)
                        -- (input :: forall x. Fn.Input x) `shouldEqual` (Fn.Input :: Fn.Input "foo")
                        -- input `shouldEqual` (Fn.Input :: Fn.Input "foo")
                        -- input `shouldEqual` _fooInput
                        -- pure unit
                        --Fn.unsafeInputToString (unsafeCoerce input) `shouldEqual` "i3"
                        -- Fn.unsafeInputToString (unsafeCoerce input) `shouldEqual` "foo"
                        -- Fn.inputToString input `shouldEqual` "foo"
                        (reflect' input) `shouldEqual` "i3"
                    Nothing -> fail "no last input was recorded"
                pure unit

            it "sending to output changes its value in refs" $ do
                tracker /\ protocol <- makeProtocol
                _ <- Process.runM protocol $ Fn.send _barOutput 7
                atBar <- liftEffect $ Protocol.outputs tracker <#> _.bar
                atBar `shouldEqual` 7
                pure unit

            it "sending to output two times its value in refs" $ do
                tracker /\ protocol <- makeProtocol
                _ <- Process.runM protocol $ Fn.send _barOutput 7
                _ <- Process.runM protocol $ Fn.send _barOutput 12
                atBar <- liftEffect $ Protocol.outputs tracker <#> _.bar
                atBar `shouldEqual` 12
                pure unit

            it "modifying state works" $ do
                tracker /\ protocol <- makeProtocol
                _ <- Process.runM protocol $ State.modify_ ((+) 42)
                state <- liftEffect $ Signal.get tracker.state
                state `shouldEqual` 44
                pure unit

            it "using inputs and outputs and modifying state works together" $ do
                tracker /\ protocol <- makeProtocol
                _ <- Process.runM protocol $ do
                    atI3 <- Fn.receive _i3Input -- get 14
                    State.modify_ ((*) atI3) -- 2 * 14
                    curState <- State.get
                    Fn.send _barOutput curState
                atBar <- liftEffect $ Protocol.outputs tracker <#> _.bar
                atBar `shouldEqual` 28
                pure unit



{-

shouldContain :: forall d. Eq d => Show d => String -> d -> Protocol.Tracker String d -> Aff Unit
shouldContain id val tracker = do
    values <- liftEffect $ Signal.get tracker
    case Map.lookup id values of
        Just otherVal ->
            if val == otherVal then pure unit
            else throwError $ error $ show val <> " /= " <> show otherVal
        Nothing ->
            throwError $ error $ "\"" <> id <> "\" was not found in tracker"


spec :: Spec Unit
spec = do

    describe "foo" $ do

        it "summing works" $ do
            p <- liftEffect $ Protocol.mkDefault [ "a" /\ 5, "b" /\ 3 ]
            let
                fn :: forall m. MonadEffect m => Fn' String String Unit m Int
                fn =
                    Fn.make' "foo" [ "a", "b" ] [ "sum" ] $ do
                        a <- Fn.receive "a"
                        b <- Fn.receive "b"
                        Fn.send "sum" $ a + b
            Fn.run 0 unit p.protocol fn
            p.outputs # shouldContain "sum" 8

        it "summing works with sendIn" $ do
            p <- liftEffect $ Protocol.mkDefault [ "a" /\ 0, "b" /\ 0 ]
            let
                fn :: forall m. MonadEffect m => Fn' String String Unit m Int
                fn =
                    Fn.make' "foo" [ "a", "b" ] [ "sum" ] $ do
                        Fn.sendIn "a" 6
                        Fn.sendIn "b" 7
                        a <- Fn.receive "a"
                        b <- Fn.receive "b"
                        Fn.send "sum" $ a + b
            Fn.run 0 unit p.protocol fn
            p.outputs # shouldContain "sum" 13
            p.inputs # shouldContain "a" 6
            p.inputs # shouldContain "b" 7

    describe "bar" $ do
        pure unit
