module Noodle.Toolkit3
  ( FamilyDef
  , FoldFamilyDefsIndexed
  , ToState
  , Toolkit
  , class HasFamilyDef
  , class HasFamilyDef'
  , from
  , name
  , nodeFamilies
  , spawn
  , toRecord
  , toStates
  , unsafeSpawn
  , unsafeSpawnR
  , familyDefs
  , familyDefsIndexed
  , mapFamilies, mapFamiliesIndexed
  , class Map, class MapI
  , class Fold, class FoldI
  , class ConvertFamilyDefTo, class ConvertFamilyDefIndexed
  , convertFamilyDef, convertFamilyDefIndexed
  )
  where

import Prelude

import Data.Semigroup (class Semigroup)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol, reifySymbol)
import Data.List as List
import Data.List (List)
import Data.Maybe as Maybe
import Data.Maybe (Maybe(..))
import Data.Foldable (foldr, class Foldable)
import Data.Traversable (sequence)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array ((:))
import Data.Array as Array
import Data.Function.Uncurried


import Effect.Console (log)
import Effect.Class (class MonadEffect, liftEffect)

import Prim.Row (class Cons, class Lacks, class Nub)
import Prim.RowList (RowList, class RowToList)
import Record as Record
import Record.Unsafe as RecordU
import Record.Extra (class Keys)
import Record.Extra as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))


import Heterogeneous.Folding as H
import Heterogeneous.Mapping as H

import Noodle.Id
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Fn2 (Fn)
import Noodle.Fn2 as Fn
import Noodle.Fn2.Process (ProcessM)
import Noodle.Fn2.Process as P

import Unsafe.Coerce (unsafeCoerce)


type Name = String


data Toolkit :: Type -> Row Type -> Type
data Toolkit gstate (families :: Row Type) = Toolkit Name (Record families)


{- type NodeDesc state is os m =
    state /\ Record is /\ Record os /\ Node state is os m -}
    {-
    { defaultState :: state
    , defaultInputs :: Record is
    , defaultOutputs :: Record os
    , node :: Node state is os m
    } -}


type FamilyDef state is os m =
    state /\ Record is /\ Record os /\ Fn state is os m


from :: forall gstate (families :: Row Type). Name -> Record families -> Toolkit gstate families
from = Toolkit


toRecord :: forall gstate (families :: Row Type). Toolkit gstate families -> Record families
toRecord (Toolkit _ tk) = tk


data ToState = ToState


class GetState from state where
    -- getState :: forall proxy s. IsSymbol s => proxy s -> from -> state
    getState :: forall f. Family' f -> from -> state


instance toState ::
  (IsSymbol sym, GetState a b) =>
  H.MappingWithIndex ToState (SProxy sym) a b where
    mappingWithIndex ToState prop a = getState (familyP prop) a


data MapFamilyDefs x = MapFamilyDefs
data MapFamilyDefsIndexed x = MapFamilyDefsIndexed

data FoldFamilyDefs x = FoldFamilyDefs

data FoldFamilyDefsIndexed x = FoldFamilyDefsIndexed


class ConvertFamilyDefTo x where
    convertFamilyDef :: forall state is os m. FamilyDef state is os m -> x


class ConvertFamilyDefIndexed x where
    convertFamilyDefIndexed :: forall f state is os m. IsSymbol f => Family' f -> FamilyDef state is os m -> x



instance mappingTo ::
  ( ConvertFamilyDefTo x ) =>
  H.Mapping (MapFamilyDefs x) (FamilyDef state is os m) x where
  mapping MapFamilyDefs = convertFamilyDef


instance mappingIndexedTo ::
  ( IsSymbol f, ConvertFamilyDefIndexed x ) =>
  H.MappingWithIndex (MapFamilyDefsIndexed x) (Family' f) (FamilyDef state is os m) x where
  mappingWithIndex MapFamilyDefsIndexed = convertFamilyDefIndexed


class Map :: forall k. RowList Type -> Row Type -> k -> Row Type -> Constraint
class
    ( RowToList families rli
    , H.MapRecordWithIndex rli (H.ConstMapping (MapFamilyDefs x)) families result
    ) <= Map rli families x result

instance mapInstances ::
    ( RowToList families rli
    , H.MapRecordWithIndex rli (H.ConstMapping (MapFamilyDefs x)) families result
    ) => Map rli families x result


class MapI :: forall k. RowList Type -> Row Type -> k -> Row Type -> Constraint
class
    ( RowToList families rli
    , H.MapRecordWithIndex rli (MapFamilyDefsIndexed x) families result
    ) <= MapI rli families x result

instance mapInstancesIndexed ::
    ( RowToList families rli
    , H.MapRecordWithIndex rli (MapFamilyDefsIndexed x) families result
    ) => MapI rli families x result


class
    ( Monoid (ff result)
    , ConvertFamilyDefTo result
    , RowToList families rl
    , H.FoldlRecord (H.ConstFolding (FoldFamilyDefs result)) (ff result) rl families (ff result)
    ) <= Fold rl ff result families

instance fold ::
    ( Monoid (ff result)
    , ConvertFamilyDefTo result
    , RowToList families rl
    , H.FoldlRecord (H.ConstFolding (FoldFamilyDefs result)) (ff result) rl families (ff result)
    ) => Fold rl ff result families

class
    ( Monoid (ff result)
    , ConvertFamilyDefIndexed result
    , RowToList families rl
    , H.FoldlRecord (FoldFamilyDefsIndexed result) (ff result) rl families (ff result)
    ) <= FoldI rl ff result families

instance foldI ::
    ( Monoid (ff result)
    , ConvertFamilyDefIndexed result
    , RowToList families rl
    , H.FoldlRecord (FoldFamilyDefsIndexed result) (ff result) rl families (ff result)
    ) => FoldI rl ff result families


instance foldDefsArr ::
    ( IsSymbol f, ConvertFamilyDefTo x )
    => H.Folding
            (FoldFamilyDefs x)
            (Array x)
            (FamilyDef state is os m)
            (Array x)
    where
    folding FoldFamilyDefs acc def = convertFamilyDef def : acc


instance foldDefsIndexedArr ::
    ( IsSymbol f, ConvertFamilyDefIndexed x )
    => H.FoldingWithIndex
            (FoldFamilyDefsIndexed x)
            (Proxy f)
            (Array x)
            (FamilyDef state is os m)
            (Array x)
    where
    foldingWithIndex FoldFamilyDefsIndexed sym acc def = convertFamilyDefIndexed (familyP sym) def : acc


mapFamilies
    :: forall gstate families families' rl x
     . Map rl families x families'
    => Toolkit gstate families
    -> Record families'
mapFamilies (Toolkit _ defs) = H.hmap (MapFamilyDefs :: MapFamilyDefs x) defs


mapFamiliesIndexed
    :: forall gstate families families' rl x
     . MapI rl families x families'
    => Toolkit gstate families
    -> Record families'
mapFamiliesIndexed (Toolkit _ defs) = H.hmapWithIndex (MapFamilyDefsIndexed :: MapFamilyDefsIndexed x) defs


familyDefs
    :: forall gstate families x rl
     . Fold rl Array x families
    => Toolkit gstate families
    -> Array x
familyDefs (Toolkit _ defs) = H.hfoldl (FoldFamilyDefs :: FoldFamilyDefs x) ([] :: Array x) defs


-- defs
--     :: forall f state is os gstate instances rla m
--      . Toolkit gstate families
--     -> Array (NodeDef state is os m)
familyDefsIndexed
    :: forall gstate families x rl
     . FoldI rl Array x families
    => Toolkit gstate families
    -> Array x
familyDefsIndexed (Toolkit _ defs) = H.hfoldlWithIndex (FoldFamilyDefsIndexed :: FoldFamilyDefsIndexed x) ([] :: Array x) defs

{-
instance mappingDefs ::
  Mapping (MapNodesDefs x) (NodeDef state is os m) x where
  mapping (MapNodesDefs x) = const []


instance map ::
    ( RL.RowToList families rln
    , MapRecordWithIndex rln (MapNodesDefs) families instances
    ) => Map rln families instances
-}

{- instance getStateNodeFn ::
  (IsSymbol sym) =>
  GetState (Node.NodeFn state d) state where
    getState sym nodeFn = 0
-}

{-
instance toStateNodeFn ::
  (IsSymbol sym) =>
  H.MappingWithIndex ToState (SProxy sym) (Node.NodeFn state d) state where
    mappingWithIndex ToState prop a = getState prop a
-}


toStates ∷ ∀ gstate (families :: Row Type) (states :: Row Type). H.HMapWithIndex ToState (Record families) (Record states) ⇒ Toolkit gstate families → Record states
toStates = H.hmapWithIndex ToState <<< toRecord


class
    ( IsSymbol f
    , Cons f x families' families
    )
    <= HasFamilyDef f families' families x -- FIXME: use newtype
instance
    ( IsSymbol f
    , Cons f x families' families
    )
    => HasFamilyDef f families' families x -- FIXME: use newtype


class
    ( Cons f x families' families
    )
    <= HasFamilyDef' f families' families x -- FIXME: use newtype
instance
    ( Cons f x families' families
    )
    => HasFamilyDef' f families' families x -- FIXME: use newtype



spawn
    :: forall f (families :: Row Type) (r' ∷ Row Type) gstate state is os m
     . MonadEffect m
    => HasFamilyDef f r' families (FamilyDef state is os m)
    => Toolkit gstate families
    -> Family f
    -> m (Node f state is os m)
spawn (Toolkit _ tk) fsym =
    Record.get fsym tk
        # makeNode
    where
      makeNode (state /\ is /\ os /\ fn) = Node.make' (family' fsym) state is os fn


unsafeSpawn
    :: forall f (families :: Row Type) (r' ∷ Row Type) gstate state is os m ks
     . MonadEffect m
    => ListsFamilies families ks
    => HasFamilyDef f r' families (FamilyDef state is os m)
    => Toolkit gstate families
    -> Family' f
    -> m (Maybe (Family f /\ Node f state is os m))
unsafeSpawn toolkit@(Toolkit name tk) family =
    if List.elem (reflect' family) $ Record.keys tk then
        let (family_ :: Family f) = reifySymbol (reflect' family) unsafeCoerce
        in Just <$> ((/\) family_) <$> (spawn toolkit family_)
    else pure Nothing


unsafeSpawnR
    :: forall f (families :: Row Type) (r' ∷ Row Type) gstate state is os m ks
     . MonadEffect m
    => ListsFamilies families ks
    => HasFamilyDef' f r' families (FamilyDef state is os m)
    => Toolkit gstate families
    -> FamilyR
    -> m (Maybe (Node f state is os m))
unsafeSpawnR toolkit@(Toolkit name tk) family =
    if List.elem (reflect' family) $ Record.keys tk then
        RecordU.unsafeGet familyStr tk
            # makeNode
            <#> Just
    else pure Nothing
    where
        familyStr = reflect' family
        (family_ :: Family' f) = reifySymbol familyStr unsafeCoerce
        makeNode (state /\ is /\ os /\ fn) = Node.make' family_ state is os fn


{-
unsafeSpawn'
    :: forall f (families :: Row Type) (r' ∷ Row Type) gstate state is os m ks
     . Keys ks
    => RowToList families ks
    => Cons f (NodeDef state is os m) r' families
    => MonadEffect m
    => Toolkit gstate families
    -> FamilyId
    -> m (Maybe (Node f state is os m))
unsafeSpawn' toolkit =
    if List.elem family $ Record.keys tk then
        Record.unsafeGet fsym tk
            # makeNode
    else pure Nothing
    where
        makeNode (state /\ is /\ os /\ fn) = Node.make' fsym state is os fn -}


name :: forall gstate families. Toolkit gstate families -> Name
name (Toolkit name _) = name


nodeFamilies :: forall ks gstate families. ListsFamilies families ks => Toolkit gstate families -> List FamilyR
nodeFamilies (Toolkit _ tk) = keysToFamiliesR (Proxy :: Proxy families)


-- nodeFamilies' :: forall ks state families. Keys ks => RowToList families ks => Toolkit state families -> List (forall f. IsSymbol f => Family f)
-- nodeFamilies' :: forall ks state families. Keys ks => RowToList families ks => Toolkit state families -> List (forall f. Family f)
-- nodeFamilies' (Toolkit _ tk) = (\key -> reifySymbol key unsafeCoerce) <$> Record.keys tk