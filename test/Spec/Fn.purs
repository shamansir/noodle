module Test.Spec.Fn where

import Prelude

import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Maybe (Maybe(..))
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
-- import Data.SOrder (type (:::), T)
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.ValueInChannel (ValueInChannel, class ToValueInChannel, class FromValueInChannel)
import Noodle.Repr.ValueInChannel (toMaybe, accept) as ViC
import Data.Newtype (class Newtype)
import Data.Newtype (wrap, unwrap) as NT

import Type.Proxy (Proxy(..))

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

-- import Noodle.Node.Shape (noInlets, noOutlets) as Shape
-- import Noodle.Node ((<~>), (+>), (<+))
-- import Noodle.Node (Node)
-- import Noodle.Node as Node
import Noodle.Fn (Fn)
import Noodle.Fn as Fn
import Noodle.Fn.Process as Fn
import Noodle.Fn.Protocol (Protocol)
import Noodle.Fn.Protocol as Protocol
import Noodle.Fn.Tracker (Tracker)
import Noodle.Fn.Tracker (outletsRec) as Tracker
import Noodle.Id (Inlet(..), Outlet(..)) as Fn
import Noodle.Id (Temperament(..))

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


{-
shouldContain :: forall state is os. String -> d -> Protocol.Tracker state is os -> Aff Unit
shouldContain id val tracker = do
    values <- liftEffect $ Ref.read tracker
    case Map.lookup id values of
        Just otherVal ->
            if val == otherVal then pure unit
            else throwError $ error $ show val <> " /= " <> show otherVal
        Nothing ->
            throwError $ error $ "\"" <> id <> "\" was not found in tracker"
-}

newtype MyRepr
    = MyRepr Int

derive instance Newtype MyRepr _


instance Show MyRepr where show = case _ of MyRepr n -> show n
instance HasFallback MyRepr where fallback = MyRepr 0
instance FromValueInChannel Int MyRepr where fromValueInChannel = NT.wrap
instance ToValueInChannel MyRepr Int where toValueInChannel = NT.unwrap >>> ViC.accept


spec :: Spec Unit
spec = do

    describe "performing functions" $ do

        it "summing works on records (tracker way)" $ do
            (tracker /\ protocol) <- liftEffect $ Protocol.makeRec unit { a : 5, b : 3 } { sum : 0 }
            let
                fn :: forall m. MonadEffect m => SumFn m
                fn =
                    Fn.make "foo" $ do
                        (a :: Int) <- Fn.receive a_in
                        (b :: Int) <- Fn.receive b_in
                        Fn.send sum_out $ a + b
            Fn.run' protocol fn
            (outlets :: Record OutletsRow) <- liftEffect $ Tracker.outletsRec tracker
            outlets.sum `shouldEqual` 8


        it "summing works on records (protocol way)" $ do
            (_ /\ protocol) <- liftEffect $ Protocol.makeRec unit { a : 5, b : 3 } { sum : 0 }
            let
                fn :: forall m. MonadEffect m => SumFn m
                fn =
                    Fn.make "foo" $ do
                        (a :: Int) <- Fn.receive a_in
                        (b :: Int) <- Fn.receive b_in
                        Fn.send sum_out $ a + b
            ((_ /\ _ /\ outlets) :: _ /\ Record InletsRow /\ Record OutletsRow) <- Fn.runRec protocol fn
            outlets.sum `shouldEqual` 8

        it "summing works with sendIn on records" $ do
            (_ /\ protocol) <- liftEffect $ Protocol.makeRec unit { a : 0, b : 0 } { sum : 0 }
            let
                fn :: forall m. MonadEffect m => SumFn m
                fn =
                    Fn.make "foo" $ do
                        Fn.sendIn a_in 6
                        Fn.sendIn b_in 7
                        a <- Fn.receive a_in
                        b <- Fn.receive b_in
                        Fn.send sum_out $ a + b
                        pure unit
            (_ /\ inlets /\ outlets :: _ /\ Record InletsRow /\ Record OutletsRow) <- Fn.runRec protocol fn
            outlets.sum `shouldEqual` 13
            inlets.a `shouldEqual` 6
            inlets.b `shouldEqual` 7


type InletsRow = ( a :: Int, b :: Int )
type OutletsRow = ( sum :: Int )


type SumFn m =
    Fn Unit ( a :: Int, b :: Int ) ( sum :: Int ) MyRepr m


a_in = Fn.Inlet :: _ "a"
b_in = Fn.Inlet :: _ "b"
sum_out = Fn.Outlet :: _ "sum"
