module Noodle.NoodleM
    ( NoodleF
    ) where


import Prelude

-- import Control.Semigroupoid ((<<<))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Free (Free, hoistFree, liftF)
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


data NoodleF state d m a
    = State (state -> a /\ state)
    | Lift (m a)
    | AddPatch Patch.Id a
    | AddNode Patch.Id Node.Family a
    -- | AddNodeFromDef Patch.Id Node.Id d (Def d) a
    -- | RemovePatch Patch.Id a
    -- | WithPatch Patch.Id (Patch d -> m (a /\ Patch d))
    -- | WithNode Patch.Id Node.Id (Node d -> m (a /\ Node d))
    -- | AddInlet Patch.Id Node.Id InletId d a
    -- | AddOutlet Patch.Id Node.Id OutletId d a
    -- | Connect Patch.Id Patch.OutletPath Patch.InletPath (Unit -> m (a /\ Node.Link))
    -- | Disconnect Patch.Id Patch.OutletPath Patch.InletPath a
    -- | DisconnectLink Node.Link
    -- | Send Patch.Id Node.Id InletId d a
    -- | Produce Patch.Id Node.Id OutletId d a
    -- | GetAtInlet Patch.Id Node.Id InletId (Unit -> m (a /\ d))
    -- | GetAtOutlet Patch.Id Node.Id OutletId (Unit -> m (a /\ d))


instance functorNoodleF :: Functor m => Functor (NoodleF state d m) where
    map f = case _ of
        State k -> State (lmap f <<< k)
        Lift m -> Lift (map f m)
        AddPatch pid a -> AddPatch pid $ f a
        AddNode pid nid a -> AddNode pid nid $ f a


newtype NoodleM state d m a = NoodleM (Free (NoodleF state d m) a)


derive newtype instance functorHalogenM :: Functor (NoodleM state d m)
derive newtype instance applyHalogenM :: Apply (NoodleM state d m)
derive newtype instance applicativeHalogenM :: Applicative (NoodleM state d m)
derive newtype instance bindHalogenM :: Bind (NoodleM state d m)
derive newtype instance monadHalogenM :: Monad (NoodleM state d m)
derive newtype instance semigroupHalogenM :: Semigroup a => Semigroup (NoodleM state d m a)
derive newtype instance monoidHalogenM :: Monoid a => Monoid (NoodleM state d m a)


instance monadEffectNoodleM :: MonadEffect m => MonadEffect (NoodleM state d m) where
  liftEffect = NoodleM <<< liftF <<< Lift <<< liftEffect


instance monadAffNoodleM :: MonadAff m => MonadAff (NoodleM state d m) where
  liftAff = NoodleM <<< liftF <<< Lift <<< liftAff


instance monadStateNoodleM :: MonadState state (NoodleM state d m) where
  state = NoodleM <<< liftF <<< State


instance monadThrowNoodleM :: MonadThrow e m => MonadThrow e (NoodleM state d m) where
  throwError = NoodleM <<< liftF <<< Lift <<< throwError


addPatch :: forall state d m. Patch.Id -> NoodleM state d m Unit
addPatch pid = NoodleM $ liftF $ AddPatch pid $ unit


addNode :: forall state d m. Patch.Id -> Node.Family -> NoodleM state d m Unit
addNode pid nfid = NoodleM $ liftF $ AddNode pid nfid $ unit


program :: forall state d m a. NoodleM state d m Unit
program = do
    addPatch "ff"
    addNode "ff" "aa"
    -- modify_ ((+) 1)
    pure unit