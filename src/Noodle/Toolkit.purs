module Noodle.Toolkit where

import Prelude

import Prim.Boolean (True, False)

import Type.Proxy (Proxy(..))
import Type.Data.List.Extra (TNil, class Put, class MapDown, mapDown, class LMap, ByReflect(..))
import Type.Data.List (List', Cons', Nil', type (:>), class Length, class IsMember)

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)

import Unsafe.Coerce (unsafeCoerce)

import Color (Color)
import Signal (Signal, (~>))
import Signal (foldp) as Signal

import Data.Symbol (class IsSymbol)
import Data.Array (catMaybes) as Array
import Data.Map (Map)
import Data.Map (empty, lookup, insert, toUnfoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (snd) as Tuple
import Data.Foldable (foldl)

import Noodle.Node (Node)
import Noodle.Node (subscribeState) as Node
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (subscribeState) as RawNode
import Noodle.Raw.Toolkit.Family (Family) as Raw
import Noodle.Id (Family, FamilyR, GroupR, familyR, ToolkitR, unsafeFamilyR, NodeR) as Id
import Noodle.Toolkit.HoldsFamily (HoldsFamily, holdFamily)
import Noodle.Toolkit.HoldsFamily (withFamily) as HF
import Noodle.Toolkit.Family (Family)
import Noodle.Toolkit.Family (familyROf, familyIdOf, spawn, toRaw) as Family
import Noodle.Raw.Toolkit.Family (id, familyIdOf, spawn, toReprableState) as RawFamily
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.Tagged (class ValueTagged) as VT
import Noodle.Ui.Palette.Mark (class Mark)


data ToolkitKey


type Name = Id.ToolkitR


data Toolkit (tk :: ToolkitKey) (families :: Families) strepr chrepr m =
    Toolkit
        Name
        (Map Id.FamilyR (HoldsFamily strepr chrepr m families)) -- FIXME: consider storing all the families in Raw format since, all the type data is in `families :: Families` and can be extracted
        (Map Id.FamilyR (Raw.Family strepr chrepr m))


empty :: forall tk strepr chrepr m. Proxy tk -> Name -> Toolkit tk TNil strepr chrepr m
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
    :: forall tk f fstate strepr is os chrepr m families families'
     . Put (F f fstate is os chrepr m) families families'
    => IsMember (F f fstate is os chrepr m) families' True
    => IsSymbol f
    => HasFallback fstate
    => StRepr fstate strepr
    => Family f fstate is os chrepr m
    -> Toolkit tk families strepr chrepr m
    -> Toolkit tk families' strepr chrepr m -- FIXME: `Put` typeclass puts new family before the others instead of putting it in the end (rename `Cons` / `Snoc` ?)
register family (Toolkit name families rawFamilies) =
    Toolkit name (Map.insert (Id.familyR $ Family.familyIdOf family) (holdFamily (Proxy :: _ families') family) (unsafeCoerce families)) rawFamilies


registerRaw
    :: forall tk strepr chrepr m families
     . Raw.Family strepr chrepr m
    -> Toolkit tk families strepr chrepr m
    -> Toolkit tk families strepr chrepr m
registerRaw rawFamily (Toolkit name families rawFamilies) =
    Toolkit name families (Map.insert (RawFamily.familyIdOf rawFamily) rawFamily rawFamilies)


registerRaw'
    :: forall tk fstate strepr chrepr m families
     . HasFallback fstate
    => StRepr fstate strepr
    => Raw.Family fstate chrepr m
    -> Toolkit tk families strepr chrepr m
    -> Toolkit tk families strepr chrepr m
registerRaw' rawFamily =
    registerRaw (RawFamily.toReprableState rawFamily)


spawn
    :: forall m tk f fstate strepr chrepr is os mp families
     . IsSymbol f
    => MonadEffect m
    => RegisteredFamily (F f fstate is os chrepr mp) families
    => Id.Family f
    -> Toolkit tk families strepr chrepr mp
    -> m (Node f fstate is os chrepr mp)
spawn familyId (Toolkit _ families _) = do
    case Map.lookup (Id.familyR familyId) families of
        Just holdsFamily -> HF.withFamily holdsFamily (spawnNode <<< unsafeCoerce)
        Nothing -> liftEffect $ throw $ "Family is not in the registry: " <> show familyId
    where
        spawnNode :: Family f fstate is os chrepr mp -> m (Node f fstate is os chrepr mp)
        spawnNode = Family.spawn


spawnRaw
    :: forall m tk strepr chrepr mp families
     . MonadEffect m
    => VT.ValueTagged chrepr
    => Id.FamilyR
    -> Toolkit tk families strepr chrepr mp
    -> m (Maybe (Raw.Node strepr chrepr mp))
spawnRaw familyR (Toolkit _ _ rawFamilies) = do
    case Map.lookup familyR rawFamilies of -- FIXME: also look up in "usual" typed families
        Just rawFamily -> Just <$> RawFamily.spawn rawFamily
        Nothing -> pure Nothing


spawnAnyRaw :: forall m tk strepr chrepr mp families
     . MonadEffect m
    => VT.ValueTagged chrepr
    => Id.FamilyR
    -> Toolkit tk families strepr chrepr mp
    -> m (Maybe (Raw.Node strepr chrepr mp))
spawnAnyRaw familyR (Toolkit _ families rawFamilies) = do
    case Map.lookup familyR rawFamilies of -- FIXME: also look up in "usual" typed families
        Just rawFamily -> Just <$> RawFamily.spawn rawFamily
        Nothing -> case Map.lookup familyR families of
                Just hf -> Just <$> HF.withFamily hf (Family.toRaw >>> RawFamily.toReprableState >>> RawFamily.spawn)
                Nothing -> pure Nothing


data MapFamilies strepr chrepr m fs = MapFamilies (Map Id.FamilyR (HoldsFamily strepr chrepr m fs))


instance IsSymbol f => LMap (MapFamilies strepr chrepr m fs) (F f fstate is os chrepr m) (Maybe (HoldsFamily strepr chrepr m fs)) where
    lmap :: MapFamilies strepr chrepr m fs -> Proxy (F f fstate is os chrepr m) -> Maybe (HoldsFamily strepr chrepr m fs)
    lmap (MapFamilies families) _ = Map.lookup (Id.familyR (Proxy :: _ f)) families


class HoldsFamilies :: Type -> Type -> (Type -> Type) -> Families -> Constraint
class    (MapDown (MapFamilies strepr chrepr m families) families Array (Maybe (HoldsFamily strepr chrepr m families))) <= HoldsFamilies strepr chrepr m families
instance (MapDown (MapFamilies strepr chrepr m families) families Array (Maybe (HoldsFamily strepr chrepr m families))) => HoldsFamilies strepr chrepr m families


mapFamilies
    :: forall x tk families strepr chrepr m
    .  HoldsFamilies strepr chrepr m families
    => (forall f fstate is os. IsSymbol f => RegisteredFamily (F f fstate is os chrepr m) families => StRepr fstate strepr => Family f fstate is os chrepr m -> x)
    -> Toolkit tk families strepr chrepr m
    -> Array x
mapFamilies f (Toolkit _ families _) =
    (\hf -> HF.withFamily hf f)
        <$> Array.catMaybes
            (mapDown (MapFamilies families) (Proxy :: _ families) :: Array (Maybe (HoldsFamily strepr chrepr m families)))


mapRawFamilies
    :: forall x tk families strepr chrepr m
    .  (Raw.Family strepr chrepr m -> x)
    -> Toolkit tk families strepr chrepr m
    -> Array x
mapRawFamilies f (Toolkit _ _ rawFamilies) =
    Map.toUnfoldable rawFamilies <#> Tuple.snd <#> f


mapAllFamilies
    :: forall x tk families strepr chrepr m
    .  HoldsFamilies strepr chrepr m families
    => (Raw.Family strepr chrepr m -> x)
    -> Toolkit tk families strepr chrepr m
    -> Array x
mapAllFamilies f (Toolkit _ families rawFamilies) =
    ((\hf -> HF.withFamily hf (Family.toRaw >>> RawFamily.toReprableState >>> f))
        <$> Array.catMaybes
            (mapDown (MapFamilies families) (Proxy :: _ families) :: Array (Maybe (HoldsFamily strepr chrepr m families))))
    <>
    (Map.toUnfoldable rawFamilies <#> Tuple.snd <#> f)


families
    :: forall tk families strepr chrepr m
    .  HoldsFamilies strepr chrepr m families
    => Toolkit tk families strepr chrepr m
    -> Array Id.FamilyR
families = mapAllFamilies RawFamily.familyIdOf


withFamily
    :: forall f x tk families strepr chrepr m
    .  IsSymbol f
    => (forall f' state' is' os'. IsSymbol f' => RegisteredFamily (F f' state' is' os' chrepr m) families => StRepr state' strepr => HasFallback state' => Family f' state' is' os' chrepr m -> x)
    -> Id.Family f
    -> Toolkit tk families strepr chrepr m
    -> Maybe x
withFamily f =
    withFamilyUnsafe f <<< Id.familyR


withFamilyUnsafe
    :: forall x tk families strepr chrepr m
    .  (forall f' state' is' os'. IsSymbol f' => RegisteredFamily (F f' state' is' os' chrepr m) families => StRepr state' strepr => HasFallback state' => Family f' state' is' os' chrepr m -> x)
    -> Id.FamilyR
    -> Toolkit tk families strepr chrepr m
    -> Maybe x
withFamilyUnsafe f familyR (Toolkit _ families _) = ado
    holdsFamily <- Map.lookup familyR families
    in HF.withFamily holdsFamily f


withRawFamily
    :: forall x tk families strepr chrepr m
    .  (Raw.Family strepr chrepr m -> x)
    -> Id.FamilyR
    -> Toolkit tk families strepr chrepr m
    -> Maybe x
withRawFamily f familyR (Toolkit _ _ rawFamilies) =
    Map.lookup familyR rawFamilies <#> f


withAnyFamily
    :: forall x tk families strepr chrepr m
    .  (Raw.Family strepr chrepr m -> x)
    -> Id.FamilyR
    -> Toolkit tk families strepr chrepr m
    -> Maybe x
withAnyFamily f familyR (Toolkit _ families rawFamilies) =
    case Map.lookup familyR rawFamilies of
        Just rawFamily -> Just $ f rawFamily
        Nothing ->
            case Map.lookup familyR families of
                Just hf -> Just $ HF.withFamily hf (Family.toRaw >>> RawFamily.toReprableState >>> f)
                Nothing -> Nothing


isKnownFamily :: forall tk families strepr chrepr m. String -> Toolkit tk families strepr chrepr m -> Maybe Id.FamilyR
isKnownFamily familyStr (Toolkit _ families rawFamilies) =
    case Map.lookup (Id.unsafeFamilyR familyStr) rawFamilies of
        Just rawFamily -> Just $ RawFamily.id rawFamily
        Nothing ->
            case Map.lookup (Id.unsafeFamilyR familyStr) families of
                Just hf -> Just $ HF.withFamily hf Family.familyROf
                Nothing -> Nothing


class IsToolkit (tk :: ToolkitKey) where
    name    :: Proxy tk -> String
    groupOf :: Proxy tk -> Id.FamilyR -> Id.GroupR


class InitPatchState (tk :: ToolkitKey) pstate m where
    initPatch :: Proxy tk -> m pstate


class FromToPatchState (tk :: ToolkitKey) pstate fstate where
    loadFromPatch :: Proxy tk -> Id.FamilyR -> pstate -> fstate -> Maybe fstate
    putInPatch :: Proxy tk -> Id.NodeR -> fstate -> pstate -> pstate


class IsToolkit tk <= MarkToolkit (tk :: ToolkitKey) where
    markGroup  :: Proxy tk -> Id.GroupR -> Color
    markFamily :: Proxy tk -> Id.GroupR -> Id.FamilyR -> Color
    -- TODO: merge in one method or even get rid of it at all: use auto-generated coloring


class HasChRepr (tk :: ToolkitKey) chrepr | tk -> chrepr
class HasStRepr (tk :: ToolkitKey) strepr | tk -> strepr