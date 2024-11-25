module Noodle.Toolkit where

import Prelude

import Prim.Boolean (True, False)

import Type.Proxy (Proxy(..))
import Type.Data.List.Extra (TNil, class Put, class MapDown, mapDown, class LMap, ByReflect(..))

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)

import Unsafe.Coerce (unsafeCoerce)

import Color (Color)

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
import Noodle.Id (Family, FamilyR, GroupR, familyR, ToolkitR) as Id
import Noodle.Toolkit.HoldsFamily (HoldsFamily, holdFamily, withFamily)
import Noodle.Toolkit.Family (Family)
import Noodle.Toolkit.Family (familyIdOf, spawn, toRaw) as F
import Noodle.Raw.Toolkit.Family (familyIdOf, spawn, toReprableState) as RF
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import Noodle.Repr (class FromToRepr)


data ToolkitKey


type Name = Id.ToolkitR


data Toolkit (tk :: ToolkitKey) (families :: Families) repr m = -- bind to
    Toolkit
        Name
        (Map Id.FamilyR (HoldsFamily repr m))
        (Map Id.FamilyR (Raw.Family repr repr m))


empty :: forall tk repr m. Proxy tk -> Name -> Toolkit tk TNil repr m
empty _ name =
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
    :: forall tk f state is os repr m families families'
     . Put (F f state is os repr m) families families'
    => IsSymbol f
    => FromToRepr state repr
    => Family f state is os repr m
    -> Toolkit tk families repr m
    -> Toolkit tk families' repr m -- FIXME: `Put` typeclass puts new family before the others instead of putting it in the end (rename `Cons` / `Snoc` ?)
register family (Toolkit name families rawFamilies) =
    Toolkit name (Map.insert (Id.familyR $ F.familyIdOf family) (holdFamily family) families) rawFamilies


registerRaw
    :: forall tk fstate repr m families
     . FromToRepr fstate repr
    => Raw.Family fstate repr m
    -> Toolkit tk families repr m
    -> Toolkit tk families repr m
registerRaw rawFamily (Toolkit name families rawFamilies) =
    Toolkit name families (Map.insert (RF.familyIdOf rawFamily) (RF.toReprableState rawFamily) rawFamilies)


spawn
    :: forall m tk f state repr is os mp families
     . IsSymbol f
    => MonadEffect m
    => RegisteredFamily (F f state is os repr mp) families
    => Id.Family f
    -> Toolkit tk families repr mp
    -> m (Node f state is os repr mp)
spawn familyId (Toolkit _ families _) = do
    case Map.lookup (Id.familyR familyId) families of
        -- TODO: Maybe lock by some constraint like `FromFamily f state is os repr m`
        -- and satisfy this constraint using method in `FamilyExistsIn (F f state is os repr m) families`
        Just holdsFamily -> withFamily holdsFamily (spawnNode <<< unsafeCoerce)
        Nothing -> liftEffect $ throw $ "Family is not in the registry: " <> show familyId
    where
        spawnNode :: Family f state is os repr mp -> m (Node f state is os repr mp)
        spawnNode = F.spawn


spawnRaw
    :: forall m tk repr mp families
     . MonadEffect m
    => Id.FamilyR
    -> Toolkit tk families repr mp
    -> m (Maybe (Raw.Node repr repr mp))
spawnRaw familyId (Toolkit _ _ rawFamilies) = do
    case Map.lookup familyId rawFamilies of -- FIXME: also look up in "usual" typed families
        -- TODO: Maybe lock by some constraint like `FromFamily f state is os repr m`
        -- and satisfy this constraint using method in `FamilyExistsIn (F f state is os repr m) families`
        Just rawFamily -> Just <$> RF.spawn rawFamily
        Nothing -> pure Nothing


data MapFamilies repr m = MapFamilies (Map Id.FamilyR (HoldsFamily repr m))


instance IsSymbol f => LMap (MapFamilies repr m) (F f state is os repr m) (Maybe (HoldsFamily repr m)) where
    lmap :: MapFamilies repr m -> Proxy (F f state is os repr m) -> Maybe (HoldsFamily repr m)
    lmap (MapFamilies families) _ = Map.lookup (Id.familyR (Proxy :: _ f)) families


class MapFamiliesImpl :: Type -> (Type -> Type) -> Families -> Constraint
class    (MapDown (MapFamilies repr m) families Array (Maybe (HoldsFamily repr m))) <= MapFamiliesImpl repr m families
instance (MapDown (MapFamilies repr m) families Array (Maybe (HoldsFamily repr m))) => MapFamiliesImpl repr m families


mapFamilies
    :: forall x tk families repr m
    .  MapFamiliesImpl repr m families
    => (forall f state is os. IsSymbol f => Family f state is os repr m -> x)
    -> Toolkit tk families repr m
    -> Array x
mapFamilies f (Toolkit _ families _) =
    (\hf -> withFamily hf f)
        <$> Array.catMaybes
            (mapDown (MapFamilies families) (Proxy :: _ families) :: Array (Maybe (HoldsFamily repr m)))


mapRawFamilies
    :: forall x tk families repr m
    .  (Raw.Family repr repr m -> x)
    -> Toolkit tk families repr m
    -> Array x
mapRawFamilies f (Toolkit _ _ rawFamilies) =
    Map.toUnfoldable rawFamilies <#> Tuple.snd <#> f


mapAllFamilies
    :: forall x tk families repr m
    .  MapFamiliesImpl repr m families
    => (Raw.Family repr repr m -> x)
    -> Toolkit tk families repr m
    -> Array x
mapAllFamilies f (Toolkit _ families rawFamilies) =
    ((\hf -> withFamily hf (F.toRaw >>> RF.toReprableState >>> f))
        <$> Array.catMaybes
            (mapDown (MapFamilies families) (Proxy :: _ families) :: Array (Maybe (HoldsFamily repr m))))
    <>
    (Map.toUnfoldable rawFamilies <#> Tuple.snd <#> f)


families
    :: forall tk families repr m
    .  MapFamiliesImpl repr m families
    => Toolkit tk families repr m
    -> Array Id.FamilyR
families = mapAllFamilies RF.familyIdOf


class IsToolkit (tk :: ToolkitKey) where
    name    :: Proxy tk -> String
    groupOf :: Proxy tk -> Id.FamilyR -> Id.GroupR


class MarkToolkit (tk :: ToolkitKey) where
    markGroup  :: Proxy tk -> Id.GroupR  -> Color
    markFamily :: Proxy tk -> Id.FamilyR -> Color