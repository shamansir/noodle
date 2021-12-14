module Noodle.NodeM
    ( NodeM
    , NodeF
    , runNodeM
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
import Noodle.Node.Shape (InletId, OutletId)
import Noodle.Node.Define as Def
import Noodle.Node.Define (Def)

import Halogen (HalogenM)


data NodeF state d m a
    = State (state -> a /\ state)
    | Lift (m a)
    | Send OutletId d a
    | Receive InletId (d -> a)


instance functorNodeF :: Functor m => Functor (NodeF state d m) where
    map f = case _ of
        State k -> State (lmap f <<< k)
        Lift m -> Lift (map f m)
        Receive iid k -> Receive iid $ map f k
        Send oid d next -> Send oid d $ f next


newtype NodeM state d m a = NodeM (Free (NodeF state d m) a)


derive newtype instance functorNodeM :: Functor (NodeM state d m)
derive newtype instance applyNodeM :: Apply (NodeM state d m)
derive newtype instance applicativeNodeM :: Applicative (NodeM state d m)
derive newtype instance bindNodeM :: Bind (NodeM state d m)
derive newtype instance monadNodeM :: Monad (NodeM state d m)
derive newtype instance semigroupNodeM :: Semigroup a => Semigroup (NodeM state d m a)
derive newtype instance monoidNodeM :: Monoid a => Monoid (NodeM state d m a)


instance monadEffectNoodleM :: MonadEffect m => MonadEffect (NodeM state d m) where
  liftEffect = NodeM <<< liftF <<< Lift <<< liftEffect


instance monadAffNoodleM :: MonadAff m => MonadAff (NodeM state d m) where
  liftAff = NodeM <<< liftF <<< Lift <<< liftAff


instance monadStateNoodleM :: MonadState state (NodeM state d m) where
  state = NodeM <<< liftF <<< State


instance monadThrowNoodleM :: MonadThrow e m => MonadThrow e (NodeM state d m) where
  throwError = NodeM <<< liftF <<< Lift <<< throwError


receive :: forall state d m. InletId -> NodeM state d m d
receive iid = NodeM $ liftF $ Receive iid identity


send :: forall state d m. OutletId -> d -> NodeM state d m Unit
send oid d = NodeM $ liftF $ Send oid d unit


program :: forall state d m. MonadEffect m => NodeM state d m Unit
program = do
    x <- receive "ee"
    n <- liftEffect $ pure 0
    -- modify_ ((+) 1)
    --pure (x + n)
    pure unit


runNodeM :: forall state d. d -> state -> Node d -> NodeM state d Aff ~> Aff
runNodeM default state node (NodeM nodeFree) = do
    stateRef <- liftEffect $ Ref.new state
    nodeRef <- liftEffect $ Ref.new node
    runNodeFreeM default stateRef nodeRef nodeFree


runNodeFreeM :: forall state d. d -> Ref state -> Ref (Node d) -> Free (NodeF state d Aff) ~> Aff
runNodeFreeM default stateRef nodeRef =
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
        go (Receive _ getV) = pure $ getV default
        go (Send _ _ next) = pure next

        getUserState = liftEffect $ Ref.read stateRef
        writeUserState nextState = liftEffect $ Ref.write nextState stateRef



-- run :: forall state d m a. state -> Network d -> NoodleM state d m a -> Effect (state /\ Network d)
-- run state nw = case _ of
--     _ -> pure $ state /\ nw
