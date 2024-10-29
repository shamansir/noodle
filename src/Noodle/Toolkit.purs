module Noodle.Toolkit where

import Prelude

import Prim.Boolean (True, False)

import Type.Proxy (Proxy(..))
import Type.Data.List.Extra (TNil, class Put, class MapDown, mapDown, class LMap, ByReflect(..))

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)

import Unsafe.Coerce (unsafeCoerce)

import Data.Symbol (class IsSymbol)
import Data.Array (catMaybes) as Array
import Data.Map (Map)
import Data.Map (empty, lookup, insert, toUnfoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (snd) as Tuple
import Data.Foldable (foldl)

import Noodle.Node (Node)
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Toolkit.Family (Family) as Raw
import Noodle.Id (Family, FamilyR, familyR) as Id
import Noodle.Toolkit.HoldsFamily (HoldsFamily, holdFamily, withFamily)
import Noodle.Toolkit.Family (Family)
import Noodle.Toolkit.Family (familyIdOf, spawn) as F
import Noodle.Raw.Toolkit.Family (familyIdOf, spawn) as RF
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)


type Name = String


data Toolkit (families :: Families) repr m =
    Toolkit
        Name
        (Map Id.FamilyR (HoldsFamily repr m))
        (Map Id.FamilyR (Raw.Family repr m))


empty :: forall repr m. Name -> Toolkit TNil repr m
empty name =
    Toolkit
        name
        Map.empty
        Map.empty


{- TODO, maybe with `unsafeCoerce`; but maybe this method is unreliable
from :: forall (families :: Families) repr m. Proxy families -> Array (HoldsFamily repr m) -> Toolkit families repr m
from _ = ?wh
-}


{- TODO, maybe with `unsafeCoerce`; but maybe this method is unreliable
registerAll
    :: forall repr m newFamilies families mergedFamilies
    .  Proxy newFamilies
    -> Array (HoldsFamily repr m)
    -> Toolkit currentFamilies repr m
    -> Toolkit mergedFamilies repr m
registerAll families toolkit =
    foldl (\tk holdsFamily -> withFamily holdsFamily (\family -> register family tk)) toolkit families
-}


register
    :: forall f state is os repr m families families'
     . Put (F f state is os repr m) families families'
    => IsSymbol f
    => Family f state is os repr m
    -> Toolkit families repr m
    -> Toolkit families' repr m -- FIXME: `Put` typeclass puts new family before the others instead of putting it in the end (rename `Cons` / `Snoc` ?)
register family (Toolkit name families rawFamilies) =
    Toolkit name (Map.insert (Id.familyR $ F.familyIdOf family) (holdFamily family) families) rawFamilies


registerRaw
    :: forall repr m families
     . Raw.Family repr m
    -> Toolkit families repr m
    -> Toolkit families repr m
registerRaw rawFamily (Toolkit name families rawFamilies) =
    Toolkit name families (Map.insert (RF.familyIdOf rawFamily) rawFamily rawFamilies)


spawn
    :: forall f state repr is os m families
     . IsSymbol f
    => MonadEffect m
    => RegisteredFamily (F f state is os repr m) families
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


spawnRaw
    :: forall repr m families
     . MonadEffect m
    => Id.FamilyR
    -> Toolkit families repr m
    -> m (Maybe (Raw.Node repr m))
spawnRaw familyId (Toolkit _ _ rawFamilies) = do
    case Map.lookup familyId rawFamilies of
        -- TODO: Maybe lock by some constraint like `FromFamily f state is os repr m`
        -- and satisfy this constraint using method in `FamilyExistsIn (F f state is os repr m) families`
        Just rawFamily -> Just <$> RF.spawn rawFamily
        Nothing -> pure Nothing


data MapFamilies repr m = MapFamilies (Map Id.FamilyR (HoldsFamily repr m))


instance IsSymbol f => LMap (MapFamilies repr m) (F f state is os repr m) (Maybe (HoldsFamily repr m)) where
    lmap :: MapFamilies repr m -> Proxy (F f state is os repr m) -> Maybe (HoldsFamily repr m)
    lmap (MapFamilies families) _ = Map.lookup (Id.familyR (Proxy :: _ f)) families


mapFamilies
    :: forall x families repr m
    .  MapDown (MapFamilies repr m) families Array (Maybe (HoldsFamily repr m))
    => (forall f state is os. IsSymbol f => Family f state is os repr m -> x)
    -> Toolkit families repr m
    -> Array x
mapFamilies f (Toolkit _ families _) =
    (\hf -> withFamily hf f)
        <$> Array.catMaybes
            (mapDown (MapFamilies families) (Proxy :: _ families) :: Array (Maybe (HoldsFamily repr m)))


mapRawFamilies
    :: forall x families repr m
    .  (Raw.Family repr m -> x)
    -> Toolkit families repr m
    -> Array x
mapRawFamilies f (Toolkit _ _ rawFamilies) =
    Map.toUnfoldable rawFamilies <#> Tuple.snd <#> f
