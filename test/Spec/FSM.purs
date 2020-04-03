module Rpd.Test.Spec.FSM
    ( spec ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Data.List as List
import Data.List ((:))
import Data.Maybe
import Data.Tuple.Nested ((/\))

import Rpd.UUID (UUID)
import Rpd.UUID as UUID

import Test.Spec (Spec, describe, it, pending', itOnly, describeOnly)
import Test.Spec.Assertions (shouldEqual, fail)

import FSM as FSM

data Error
  = ErrorOne
  | ErrorTwo


data Model
  = Empty
  | HoldsString String
  | HoldsAction Action
  | HoldsUUID UUID


emptyModel :: Model
emptyModel = Empty


data Action
  = NoOp
  | ActionOne
  | StoreUUID UUID


spec :: Spec Unit
spec = do

  describeOnly "FSM" do

    describe "creating" do

      it "is easy to create" do
        let fsm = FSM.make unit (\_ _ -> unit /\ [])
        pure unit

      it "is easy to run" do
        let fsm = FSM.make unit (\_ _ -> unit /\ [])
        _ <- liftEffect $ FSM.run fsm List.Nil
        pure unit

      it "is easy to run with some actions" do
        let fsm = FSM.make unit (\_ _ -> unit /\ [])
        _ <- liftEffect $ FSM.run fsm $ List.singleton NoOp
        pure unit

    describe "updating" do

      it "calls the update function" do
        let
          myFsm = FSM.make emptyModel (\_ _ -> HoldsString "foo" /\ [])
        val <- liftEffect $ do
          ref <- Ref.new Empty
          _ <- FSM.runAndSubscribe myFsm (flip Ref.write ref)
                  $ List.singleton NoOp
          Ref.read ref
        val `shouldEqual` (HoldsString "foo")

      it "the action is actually sent" do
        let
          myFsm = FSM.make emptyModel (\action _ -> HoldsAction action /\ [])
        val <- liftEffect $ do
          ref <- Ref.new Empty
          _ <- FSM.runAndSubscribe myFsm (flip Ref.write ref)
                  $ List.singleton ActionOne
          Ref.read ref
        val `shouldEqual` (HoldsAction ActionOne)

      it "receives all the actions which were sent" do
        let
          myFsm = FSM.make emptyModel (\action _ -> HoldsAction action /\ [])
        val <- liftEffect $ do
          ref <- Ref.new Empty
          _ <- FSM.runAndSubscribe myFsm (flip Ref.write ref)
                  $ (ActionOne : NoOp : List.Nil)
          Ref.read ref
        val `shouldEqual` (HoldsAction NoOp)

    describe "effects" do

      it "performs the effect from the update function" do
        let
          updateF NoOp model = model /\ [ UUID.new >>= pure <<< StoreUUID ]
          updateF (StoreUUID uuid) _ = HoldsUUID uuid /\ []
          updateF _ model = model /\ []
          myFsm = FSM.make emptyModel updateF
        val <- liftEffect $ do
          ref <- Ref.new Empty
          _ <- FSM.runAndSubscribe myFsm (flip Ref.write ref)
                  $ List.singleton NoOp
          Ref.read ref
        case val of
          (HoldsUUID _) -> pure unit
          _ -> fail "should contain UUID in the model"


instance showAction :: Show Action where
    show NoOp = "NoOp"
    show ActionOne = "ActionOne"
    show (StoreUUID u) = "Store UUID" <> show u


instance showModel :: Show Model where
    show Empty = "Empty"
    show (HoldsString s) = "HoldsString: " <> s
    show (HoldsAction a) = "HoldsAction: " <> show a
    show (HoldsUUID u) = "HoldsUUID: " <> show u


instance eqAction :: Eq Action where
    eq NoOp NoOp = true
    eq ActionOne ActionOne = true
    eq (StoreUUID uuidA) (StoreUUID uuidB) = uuidA == uuidB
    eq _ _ = false


instance eqModel :: Eq Model where
    eq Empty Empty = true
    eq (HoldsString sA) (HoldsString sB) = sA == sB
    eq (HoldsAction aA) (HoldsAction aB) = aA == aB
    eq (HoldsUUID uA) (HoldsUUID uB) = uA == uB
    eq _ _ = false
