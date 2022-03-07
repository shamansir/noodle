module Noodle.PatchM
    ( PatchM
    , PatchF
    , runPatchM
    ) where


import Prelude

-- import Control.Semigroupoid ((<<<))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Ref as Ref
import Effect.Ref (Ref)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Free (Free, hoistFree, liftF, runFreeM, foldFree)
import Control.Monad.State (modify, modify_)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Error.Class (class MonadThrow, throwError)

import Data.Bifunctor (lmap)
import Data.Tuple.Nested (type (/\), (/\))

import Noodle.Patch as Patch
import Noodle.Patch (Patch)
import Noodle.Node as Node
import Noodle.Node (Node)
-- import Noodle.Node.Shape (InletId, OutletId)
-- import Noodle.Node.Define as Def
-- import Noodle.Node.Define (Def)

import Halogen (HalogenM)


data PatchF state d m a
    = State (state -> a /\ state)
    | Lift (m a)
    | AddNode Node.Family a
    | RemoveNode Node.Id a


instance functorPatchF :: Functor m => Functor (PatchF state d m) where
    map f = case _ of
        State k -> State (lmap f <<< k)
        Lift m -> Lift (map f m)
        AddNode nf next -> AddNode nf $ f next
        RemoveNode ni next -> RemoveNode ni $ f next


newtype PatchM state d m a = PatchM (Free (PatchF state d m) a)


derive newtype instance functorPatchM :: Functor (PatchM state d m)
derive newtype instance applyPatchM :: Apply (PatchM state d m)
derive newtype instance applicativePatchM :: Applicative (PatchM state d m)
derive newtype instance bindPatchM :: Bind (PatchM state d m)
derive newtype instance monadPatchM :: Monad (PatchM state d m)
derive newtype instance semigroupPatchM :: Semigroup a => Semigroup (PatchM state d m a)
derive newtype instance monoidPatchM :: Monoid a => Monoid (PatchM state d m a)


instance monadEffectNoodleM :: MonadEffect m => MonadEffect (PatchM state d m) where
  liftEffect = PatchM <<< liftF <<< Lift <<< liftEffect


instance monadAffNoodleM :: MonadAff m => MonadAff (PatchM state d m) where
  liftAff = PatchM <<< liftF <<< Lift <<< liftAff


instance monadStateNoodleM :: MonadState state (PatchM state d m) where
  state = PatchM <<< liftF <<< State


instance monadThrowNoodleM :: MonadThrow e m => MonadThrow e (PatchM state d m) where
  throwError = PatchM <<< liftF <<< Lift <<< throwError


{-
program :: forall state d m. MonadEffect m => PatchM state d m Unit
program = do
    x <- receive "ee"
    n <- liftEffect $ pure 0
    -- modify_ ((+) 1)
    --pure (x + n)
    pure unit -}


runPatchM :: forall state node_state d. d -> state -> Patch node_state Aff d -> PatchM state d Aff ~> Aff
runPatchM default state patch (PatchM patchFree) = do
    stateRef <- liftEffect $ Ref.new state
    patchRef <- liftEffect $ Ref.new patch
    runPatchFreeM default stateRef patchRef patchFree


runPatchFreeM :: forall state node_state m d. d -> Ref state -> Ref (Patch node_state m d) -> Free (PatchF state d Aff) ~> Aff
runPatchFreeM default stateRef patchRef =
    --foldFree go-- (go stateRef)
    runFreeM go
    where
        go (State f) = do
            state <- getUserState
            case f state of
                next /\ nextState -> do
                    writeUserState nextState
                    pure next
        go (Lift m) = m
        go (AddNode _ next) = pure next
        go (RemoveNode _ next) = pure next

        getUserState = liftEffect $ Ref.read stateRef
        writeUserState nextState = liftEffect $ Ref.write nextState stateRef



-- run :: forall state d m a. state -> Network d -> NoodleM state d m a -> Effect (state /\ Network d)
-- run state nw = case _ of
--     _ -> pure $ state /\ nw
