module Noodle.Test.Spec.Util
    ( spec
    ) where

import Prelude

import Effect.Class (liftEffect)

import Data.String as String

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


import Noodle.UUID as UUID
-- import Noodle.API.Covered (Covered(..))
-- import Noodle.API.Covered as Covered


spec :: Spec Unit
spec = do
  describe "UUID generation" do
    it "does what it says" do
      uuid <- liftEffect $ UUID.new
      _ <- 36 `shouldEqual` String.length (UUID.toRawString uuid)
      pure unit

  {-
  describeOnly "covered" do

    it "covers the state" do
      let
        covered = Covered.cover "a"

      Covered.uncover covered `shouldEqual` (([] :: Array String) /\ Just "a")

    it "collects errors" do
      let
        covered = Covered.notice "err"

      Covered.uncover covered `shouldEqual` (["err"] /\ (Nothing :: Maybe String))

    it "collects several errors" do
      let
        covered = do
          x <- Covered.hoistOne "err1"
          Covered.hoistOne "err2" x

      Covered.uncover covered `shouldEqual` (["err1", "err2"] /\ (Nothing :: Maybe String))

    it "adds an error to the last state" do
      let
        covered = do
          _ <- Covered.cover "a"
          Covered.notice "err"

      Covered.uncover covered `shouldEqual` (["err"] /\ Just "a")


    it "adds errors to the last state, even when it was at start" do
      let
        covered = do
          _ <- Covered.cover "a"
          _ <- Covered.notice "err1"
          Covered.notice "err2"

      Covered.uncover covered `shouldEqual` (["err1", "err2"] /\ Just "a")


    it "adds errors to the last state, in the middle" do
      let
        covered = do
          _ <- Covered.notice "err1"
          _ <- Covered.cover "a"
          Covered.notice "err2"

      Covered.uncover covered `shouldEqual` (["err1", "err2"] /\ Just "a")

    it "adds errors to the last state, in the middle after another one" do
      let
        covered = do
          _ <- Covered.cover "a"
          _ <- Covered.notice "err1"
          _ <- Covered.cover "b"
          Covered.notice "err2"

      Covered.uncover covered `shouldEqual` (["err1", "err2"] /\ Just "b")

    it "adds errors to the last state, in the end" do
      let
        covered = do
          _ <- Covered.cover "a"
          _ <- Covered.notice "err1"
          _ <- Covered.cover "b"
          _ <- Covered.notice "err2"
          Covered.cover "c"

      Covered.uncover covered `shouldEqual` (["err1", "err2"] /\ Just "c")


    it "if the state is returned, covers it instead" do
      let
        covered = do
          _ <- Covered.cover "a"
          _ <- Covered.notice "err1"
          b <- Covered.cover "b"
          _ <- Covered.notice "err2"
          _ <- Covered.cover "c"
          pure b

      Covered.uncover covered `shouldEqual` (["err1", "err2"] /\ Just "c")
    -}
