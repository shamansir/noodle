module Rpd.Test.Spec.FSM
    ( spec ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Console as Console

import Data.List as List
import Data.List ((:), List(..))
import Data.Maybe
import Data.Tuple.Nested ((/\))
import Data.Covered as Covered
import Data.Covered (Covered(..))

import Rpd.UUID (UUID)
import Rpd.UUID as UUID

import Test.Spec (Spec, describe, it, pending', pending)
import Test.Spec.Assertions (shouldEqual, fail)

import FSM as FSM
import FSM (FSM, doNothing, single, batch)
import FSM.Covered (CoveredFSM)


data Error
  = ErrorOne
  | ErrorTwo


data Model
  = Empty
  | HoldsString String
  | HoldsAction Action
  | HoldsUUID UUID
  | HoldsError Error


emptyModel :: Model
emptyModel = Empty


data Action
  = NoOp
  | ActionOne
  | StoreUUID UUID


spec :: Spec Unit
spec = do

  describe "FSM" do

    describe "creating" do

      it "is easy to create" do
        let (fsm :: FSM Action Unit) = FSM.make (\_ _ -> unit /\ doNothing)
        pure unit

      it "is easy to run" do
        let (fsm :: FSM Action Unit) = FSM.make (\_ _ -> unit /\ doNothing)
        _ <- liftEffect $ FSM.run fsm unit List.Nil
        pure unit

      it "is easy to run with some actions" do
        let fsm = FSM.make (\_ _ -> unit /\ doNothing)
        _ <- liftEffect $ FSM.run fsm unit $ List.singleton NoOp
        pure unit

    describe "updating" do

      it "calls the update function" do
        let
          (myFsm ::FSM Action Model) = FSM.make (\_ _ -> HoldsString "foo" /\ doNothing)
        lastModel <- liftEffect $ do
          ref <- Ref.new Empty
          _ <- FSM.run' myFsm emptyModel (flip Ref.write ref)
                  $ List.singleton NoOp
          Ref.read ref
        lastModel `shouldEqual` (HoldsString "foo")

      it "the action is actually sent" do
        let
          myFsm = FSM.make (\action _ -> HoldsAction action /\ doNothing)
        lastModel <- liftEffect $ do
          ref <- Ref.new Empty
          _ <- FSM.run' myFsm emptyModel (flip Ref.write ref)
                  $ List.singleton ActionOne
          Ref.read ref
        lastModel `shouldEqual` (HoldsAction ActionOne)

      it "receives all the actions which were sent" do
        let
          myFsm = FSM.make (\action _ -> HoldsAction action /\ doNothing)
        lastModel <- liftEffect $ do
          ref <- Ref.new Empty
          _ <- FSM.run' myFsm emptyModel (flip Ref.write ref)
                  $ (ActionOne : NoOp : List.Nil)
          Ref.read ref
        lastModel `shouldEqual` (HoldsAction NoOp)

    describe "effects" do

      it "performs the effect from the update function" do
        let
          updateF NoOp model = model /\ (UUID.new >>= single <<< StoreUUID)
          updateF (StoreUUID uuid) _ = HoldsUUID uuid /\ doNothing
          updateF _ model = model /\ doNothing
          myFsm = FSM.make updateF
        lastModel <- liftEffect $ do
          ref <- Ref.new Empty
          _ <- FSM.run' myFsm emptyModel (flip Ref.write ref)
                  $ List.singleton NoOp
          Ref.read ref
        case lastModel of
          (HoldsUUID _) -> pure unit
          _ -> fail $ "should contain UUID in the model, but " <> show lastModel

      it "works when asking to perform several actions from effectful part of `update`" do
        let
          updateF ActionOne model =
            model /\ (UUID.new >>= \uuid -> batch $ NoOp : StoreUUID uuid : Nil)
          updateF (StoreUUID uuid) _ = HoldsUUID uuid /\ doNothing
          updateF _ model = model /\ doNothing
          myFsm = FSM.make updateF
        lastModel <- liftEffect $ do
          ref <- Ref.new Empty
          _ <- FSM.run' myFsm emptyModel (flip Ref.write ref)
                  $ (ActionOne : List.Nil)
          Ref.read ref
        case lastModel of
          (HoldsUUID _) -> pure unit
          _ -> fail $ "should contain UUID in the model, but " <> show lastModel

      it "effectful actions should rule over the model" do
        let
          updateF ActionOne model =
            HoldsString "fail" /\
              (UUID.new >>= single <<< StoreUUID)
          updateF (StoreUUID uuid) model =
            HoldsUUID uuid /\ doNothing
          updateF _ model =
            model /\ doNothing
          myFsm = FSM.make updateF
        lastModel <- liftEffect $ do
          ref <- Ref.new Empty
          _ <- FSM.run' myFsm emptyModel (flip Ref.write ref)
                  $ (ActionOne : List.Nil)
          Ref.read ref
        case lastModel of
          (HoldsUUID _) -> pure unit
          _ -> fail $ "should contain UUID in the model, but " <> show lastModel

    describe "folding" do

      it "folds the models to the very last state" do
        let
          updateF NoOp _ = HoldsString "NoOp" /\ doNothing
          updateF ActionOne _ = HoldsString "ActionOne" /\ doNothing
          updateF _ model = model /\ doNothing
          myFsm = FSM.make updateF
        (lastModel /\ _) <- liftEffect $ FSM.fold myFsm emptyModel $ (NoOp : ActionOne : List.Nil)
        lastModel `shouldEqual` HoldsString "ActionOne"

      it "folds the models to the very last state with effects as well" do
        let
          updateF NoOp model = model /\ (UUID.new >>= single <<< StoreUUID)
          updateF (StoreUUID uuid) _ = HoldsUUID uuid /\ doNothing
          updateF _ model = model /\ doNothing
          myFsm = FSM.make updateF
        (lastModel /\ _) <- liftEffect $ FSM.fold myFsm emptyModel $ (NoOp : ActionOne : List.Nil)
        case lastModel of
          (HoldsUUID _) -> pure unit
          _ -> fail $ "should contain UUID in the model, but " <> show lastModel

      it "effectful actions should rule over the model" do
        let
          updateF ActionOne model =
            HoldsString "fail" /\
              (UUID.new >>= single <<< StoreUUID)
          updateF (StoreUUID uuid) model =
            HoldsUUID uuid /\ doNothing
          updateF _ model =
            model /\ doNothing
          myFsm = FSM.make updateF
        (lastModel /\ _) <- liftEffect $ FSM.fold myFsm emptyModel $ (ActionOne : List.Nil)
        case lastModel of
          (HoldsUUID _) -> pure unit
          _ -> fail $ "should contain UUID in the model, but " <> show lastModel

    describe "pushing actions" do

      it "works after running the FSM" do
        let
          (myFsm :: FSM Action Model) = FSM.make (\_ _ -> HoldsString "foo" /\ doNothing)
        lastModel <- liftEffect $ do
          ref <- Ref.new Empty
          { pushAction } <- FSM.run' myFsm emptyModel (flip Ref.write ref)
                  $ List.Nil
          pushAction NoOp
          Ref.read ref
        lastModel `shouldEqual` (HoldsString "foo")

    describe "stopping" do

      pending "TODO"

  describe "CoveredFSM" do

    it "passes error through update cycle" do
        let
          (myCovererdFsm :: CoveredFSM Error Action Model) =
              FSM.make (\_ _ -> Covered.cover Empty ErrorOne /\ doNothing)
        lastModel <- liftEffect $ do
          ref <- Ref.new $ Covered.carry Empty
          _ <- FSM.run' myCovererdFsm (Covered.carry emptyModel) (flip Ref.write ref)
                  $ NoOp : List.Nil
          Ref.read ref
        case lastModel of
          Recovered ErrorOne _ -> pure unit
          _ -> fail $ "does not contain ErrorOne, but " <> show lastModel

    it "keeps the latest error" do
        let
          updateF NoOp _ = Covered.cover Empty ErrorOne /\ doNothing
          updateF ActionOne _ = Covered.cover Empty ErrorTwo /\ doNothing
          updateF _ _ = Covered.carry Empty /\ doNothing
          (myCovererdFsm :: CoveredFSM Error Action Model) = FSM.make updateF
        lastModel <- liftEffect $ do
          ref <- Ref.new $ Covered.carry Empty
          _ <- FSM.run' myCovererdFsm (Covered.carry emptyModel) (flip Ref.write ref)
                  $ NoOp : ActionOne : List.Nil
          Ref.read ref
        case lastModel of
          Recovered ErrorTwo _ -> pure unit
          _ -> fail $ "does not contain ErrorTwo, but " <> show lastModel

    it "provides the way to collect errors" do
        let
          updateF' NoOp c = Covered.cover Empty [ ErrorOne ] /\ doNothing
          updateF' ActionOne _ = Covered.cover Empty [ ErrorTwo ] /\ doNothing
          updateF' _ _ = Covered.carry Empty /\ doNothing
          updateF action prevModel =
            let
              nextModel /\ effects = updateF' action prevModel
            in
              prevModel `Covered.joinErrors` nextModel /\ effects
          (myCovererdFsm :: CoveredFSM (Array Error) Action Model) = FSM.make updateF
        lastModel <- liftEffect $ do
          ref <- Ref.new $ Covered.carry Empty
          _ <- FSM.run' myCovererdFsm (Covered.carry emptyModel) (flip Ref.write ref)
                  $ NoOp : ActionOne : List.Nil
          Ref.read ref
        case lastModel of
          Recovered [ ErrorOne, ErrorTwo ] _ -> pure unit
          _ -> fail $ "does not contain [ ErrorOne, ErrorTwo ], but " <> show lastModel


instance showAction :: Show Action where
    show NoOp = "NoOp"
    show ActionOne = "ActionOne"
    show (StoreUUID u) = "Store UUID" <> show u


instance showModel :: Show Model where
    show Empty = "Empty"
    show (HoldsString s) = "HoldsString: " <> s
    show (HoldsAction a) = "HoldsAction: " <> show a
    show (HoldsUUID u) = "HoldsUUID: " <> show u
    show (HoldsError e) = "HoldsError: " <> show e


instance showError :: Show Error where
    show ErrorOne = "ErrorOne"
    show ErrorTwo = "ErrorTwo"


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
    eq (HoldsError eA) (HoldsError eB) = eA == eB
    eq _ _ = false


derive instance eqError :: Eq Error
