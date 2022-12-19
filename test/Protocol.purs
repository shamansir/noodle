module Test.Protocol where

import Prelude

import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Symbol (reflectSymbol)

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
import Noodle.Fn2.Flow (Input(..), Output(..), inputId, outputId) as Fn

import Unsafe.Coerce (unsafeCoerce)

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


type TestInputs = ( foo :: String, i2 :: Boolean )
type TestOutputs = ( bar :: Int, o2 :: Boolean )


_fooInput = Fn.Input "foo" :: Fn.Input "foo"
_i3Input = Fn.Input "i3" :: Fn.Input "i3"
_barOutput = Fn.Output "bar" :: Fn.Output "bar"


testSend ∷ ∀ state is m. Int → ProcessM state is TestOutputs m Unit
testSend int = Fn.send _barOutput int


testSendIn ∷ ∀ state os m. String → ProcessM state TestInputs os m Unit
testSendIn str = Fn.sendIn _fooInput str


testReceive ∷ ∀ state os m. ProcessM state TestInputs os m String
testReceive = Fn.receive _fooInput


spec :: Spec Unit
spec =
    do

        let protocolOnRefs = Protocol.onRefs 2 { foo : "foo", i2 : false, i3 : 14 } { bar : 4, o2 : true }

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
                lastInput <- liftEffect $ Ref.read protocolS.lastInput
                case lastInput of
                    Just input ->
                        let inputSymbol = Fn.inputId $ unsafeCoerce input
                        in inputSymbol `shouldEqual` "foo"
                    Nothing -> fail "no last input was recorded"
                pure unit

            it "sending to input updates twice last input ref anyway" $ do
                protocolS <- protocolOnRefs
                _ <- Process.runM protocolS.protocol $ Fn.sendIn _fooInput "foobar"
                _ <- Process.runM protocolS.protocol $ Fn.sendIn _i3Input 12
                lastInput <- liftEffect $ Ref.read protocolS.lastInput
                case lastInput of
                    Just input ->
                        let inputSymbol = Fn.inputId $ unsafeCoerce input
                        in inputSymbol `shouldEqual` "i3"
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
