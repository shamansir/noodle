module Rpd.Test.Spec.FSM
    ( spec ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Data.List as List
import Data.Maybe
import Data.Tuple.Nested ((/\))

import Rpd.UUID

import Test.Spec (Spec, describe, it, pending', itOnly, describeOnly)
import Test.Spec.Assertions (shouldEqual, fail)

import FSM as FSM

data Error
  = ErrorOne
  | ErrorTwo


type Model =
  String


emptyModel :: Model
emptyModel = ""


data Action
  = NoOp
  | ActionOne



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
          myFsm = FSM.make emptyModel (\_ _ -> "foo" /\ [])
        val <- liftEffect $ do
          ref <- Ref.new ""
          _ <- FSM.runAndSubscribe myFsm (flip Ref.write ref)
                  $ List.singleton NoOp
          Ref.read ref
        val `shouldEqual` "foo"
        pure unit
