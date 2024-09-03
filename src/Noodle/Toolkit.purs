module Noodle.Toolkit where

import Prelude

import Type.Proxy (Proxy(..))

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)

import Unsafe.Coerce (unsafeCoerce)

import Data.Symbol (class IsSymbol)
import Data.Array ((:))
import Data.Map (Map)
import Data.Map (empty, lookup, insert) as Map
import Data.Maybe (Maybe(..))

import Noodle.Node (Node, RawNode)
import Noodle.Id (Family, FamilyR, familyR) as Id
import Noodle.Toolkit.HoldsFamily (HoldsFamily, HoldsRawFamily, holdFamily, holdRawFamily, withFamily, withRawFamily)
import Noodle.Toolkit.Families (Families, Family, RawFamily, class FamilyExistsIn, class PutFamily, F, FNil)
import Noodle.Toolkit.Families (familyIdOf, familyRIdOf, spawn, spawnRaw) as F


type Name = String


data Toolkit (families :: Families) repr m = Toolkit Name (Map Id.FamilyR (HoldsFamily repr m)) (Map Id.FamilyR (HoldsRawFamily repr m))


empty :: forall repr m. Name -> Toolkit FNil repr m
empty name =
    Toolkit
        name
        Map.empty
        Map.empty



register
    :: forall f state is os repr m families families'
     . PutFamily (F f state is os repr m) families families'
    => IsSymbol f
    => Family f state is os repr m
    -> Toolkit families repr m
    -> Toolkit families' repr m
register family (Toolkit name families rawFamilies) =
    Toolkit name (Map.insert (Id.familyR $ F.familyIdOf family) (holdFamily family) families) rawFamilies


registerRaw
    :: forall state repr m families
     . RawFamily state repr m
    -> Toolkit families repr m
    -> Toolkit families repr m
registerRaw rawFamily (Toolkit name families rawFamilies) =
    Toolkit name families (Map.insert (F.familyRIdOf rawFamily) (holdRawFamily rawFamily) rawFamilies)


spawn
    :: forall f state repr is os m families
     . IsSymbol f
    => MonadEffect m
    => FamilyExistsIn (F f state is os repr m) families
    => Id.Family f
    -> Toolkit families repr m
    -> m (Node f state is os repr m)
spawn familyId (Toolkit _ families _) = do
    case Map.lookup (Id.familyR familyId) families of
        -- TODO: Maybe lock by some constraint like `FromFamily f state is os repr m`
        -- and satisfy this constraint using method in `FamilyExistsIn (F f state is os repr m) families`
        Just holdsFamily -> withFamily holdsFamily (spawnNode <<< unsafeCoerce)
        Nothing -> liftEffect $ throw $ "Family is not in the registry: " <> show familyId
    where
        spawnNode :: Family f state is os repr m -> m (Node f state is os repr m)
        spawnNode = F.spawn


{-
spawnRaw
    :: forall state repr m families
     . MonadEffect m
    => Proxy state
    -> Id.FamilyR
    -> Toolkit families repr m
    -> m (Maybe (RawNode state repr m))
spawnRaw _ familyId (Toolkit _ _ rawFamilies) = do
    case Map.lookup familyId rawFamilies of
        -- TODO: Maybe lock by some constraint like `FromFamily f state is os repr m`
        -- and satisfy this constraint using method in `FamilyExistsIn (F f state is os repr m) families`
        Just holdsRawFamily -> Just <$> withRawFamily holdsRawFamily spawnRawNode
        Nothing -> pure Nothing
        -- Nothing -> liftEffect $ throw $ "Family is not in the registry: " <> show familyId
    where
        spawnRawNode :: RawFamily state repr m -> m (RawNode state repr m)
        spawnRawNode = F.spawnRaw
-}


-- spawnRaw