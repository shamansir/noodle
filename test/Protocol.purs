module Test.Protocol where

import Prelude

import Data.Map as Map
import Data.Array as Array
import Data.Map.Extra (type (/->))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Symbol (reflectSymbol, class IsSymbol)
import Prim.Symbol (class Compare)
import Record.Extra (keys)
import Effect.Console (log) as Console

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
import Noodle.Fn2.Flow (Input(..), Output(..), iToSProxy, oToSProxy, inputIdToString) as Fn

import Unsafe.Coerce (unsafeCoerce)

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT

import Type.Data.Symbol (compare)


type TestInputs = ( foo :: String, i2 :: Boolean )
type TestOutputs = ( bar :: Int, o2 :: Boolean )


_fooInput = Fn.Input :: Fn.Input "foo"
_i3Input = Fn.Input :: Fn.Input "i3"
_barOutput = Fn.Output :: Fn.Output "bar"



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
        let protocolOnRefs = Protocol.onRefs initialState initialInputs initialOutputs

        describe "keys" $ do

            it "we can get keys of the record" $ do
                (Array.toUnfoldable [ "foo", "i2", "i3" ]) `shouldEqual` Fn.inputsOf initialInputs

            it "we can get keys of the inputs" $ do
                protocolS <- protocolOnRefs
                inputs <- liftEffect $ Ref.read protocolS.inputs
                (Array.toUnfoldable [ "foo", "i2", "i3" ]) `shouldEqual` Fn.inputsOf inputs

        describe "protocol" $ do

            it "values stay initial until changed" $ do
                protocolS <- protocolOnRefs
                curState <- liftEffect $ Ref.read protocolS.state
                curState `shouldEqual` 2
                atFoo <- liftEffect $ Ref.read protocolS.inputs <#> _.foo
                atFoo `shouldEqual` "foo"
                atBar <- liftEffect $ Ref.read protocolS.outputs <#> _.bar
                atBar `shouldEqual` 4
                pure unit

        describe "process" $ do

            it "sending to input changes its value in refs" $ do
                protocolS <- protocolOnRefs
                _ <- Process.runM protocolS.protocol $ Fn.sendIn _fooInput "foobar"
                atFoo <- liftEffect $ Ref.read protocolS.inputs <#> _.foo
                atFoo `shouldEqual` "foobar"
                pure unit

            it "sending to input two times its value in refs" $ do
                protocolS <- protocolOnRefs
                _ <- Process.runM protocolS.protocol $ Fn.sendIn _fooInput "foobar"
                _ <- Process.runM protocolS.protocol $ Fn.sendIn _fooInput "barfoo"
                atFoo <- liftEffect $ Ref.read protocolS.inputs <#> _.foo
                atFoo `shouldEqual` "barfoo"
                pure unit

            it "sending to input updates last input ref" $ do
                protocolS <- protocolOnRefs
                _ <- Process.runM protocolS.protocol $ Fn.sendIn _fooInput "foobar"
                (lastInput :: Protocol.CurIVal) <- liftEffect $ Ref.read protocolS.lastInput
                case lastInput of
                    Just input ->
                        -- let (str :: String) = unsafeCoerce input
                        -- in
                        -- liftEffect $ Console.log str
                        -- pure unit
                        -- Fn.unsafeInputToString (unsafeCoerce input) `shouldEqual` "foo"
                        -- Fn.inputToString input `shouldEqual` "foo"

                        -- pure unit
                        (Fn.inputIdToString input) `shouldEqual` "foo"
                    Nothing -> fail "no last input was recorded"
                pure unit

            it "sending to input updates twice last input ref anyway" $ do
                protocolS <- protocolOnRefs
                _ <- Process.runM protocolS.protocol $ Fn.sendIn _fooInput "foobar"
                _ <- Process.runM protocolS.protocol $ Fn.sendIn _i3Input 12
                -- (lastInput :: Maybe (forall x. IsSymbol x => Fn.Input x)) <- liftEffect $ Ref.read protocolS.lastInput
                (lastInput :: _) <- liftEffect $ Ref.read protocolS.lastInput
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
                        (Fn.inputIdToString input) `shouldEqual` "i3"
                    Nothing -> fail "no last input was recorded"
                pure unit

            it "sending to output changes its value in refs" $ do
                protocolS <- protocolOnRefs
                _ <- Process.runM protocolS.protocol $ Fn.send _barOutput 7
                atBar <- liftEffect $ Ref.read protocolS.outputs <#> _.bar
                atBar `shouldEqual` 7
                pure unit

            it "sending to output two times its value in refs" $ do
                protocolS <- protocolOnRefs
                _ <- Process.runM protocolS.protocol $ Fn.send _barOutput 7
                _ <- Process.runM protocolS.protocol $ Fn.send _barOutput 12
                atBar <- liftEffect $ Ref.read protocolS.outputs <#> _.bar
                atBar `shouldEqual` 12
                pure unit

            it "modifying state works" $ do
                protocolS <- protocolOnRefs
                _ <- Process.runM protocolS.protocol $ State.modify_ ((+) 42)
                state <- liftEffect $ Ref.read protocolS.state
                state `shouldEqual` 44
                pure unit

            it "using inputs and outputs and modifying state works together" $ do
                protocolS <- protocolOnRefs
                _ <- Process.runM protocolS.protocol $ do
                    atI3 <- Fn.receive _i3Input -- get 14
                    State.modify_ ((*) atI3) -- 2 * 14
                    curState <- State.get
                    Fn.send _barOutput curState
                atBar <- liftEffect $ Ref.read protocolS.outputs <#> _.bar
                atBar `shouldEqual` 28
                pure unit



{-

shouldContain :: forall d. Eq d => Show d => String -> d -> Protocol.Tracker String d -> Aff Unit
shouldContain id val tracker = do
    values <- liftEffect $ Ref.read tracker
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
