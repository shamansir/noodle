module Noodle.Toolkit3.MapsFolds where


import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.Array ((:))
import Data.Array as Array
import Data.Symbol (SProxy)
import Data.List (List)

import Type.Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(..))


import Record.Extra (class Keys)
import Record.Extra as Record
import Prim.RowList as RL

import Heterogeneous.Mapping as HM
import Heterogeneous.Folding as HF

import Noodle.Id (Family', familyP)
import Noodle.Fn2 (Fn)


{- Helper types -}

type FamilyDef state (is :: Row Type) (os :: Row Type) (m :: Type -> Type) =
    state /\ Record is /\ Record os /\ Fn state is os m


inputsFromDef :: forall rl state (is :: Row Type) os m. Keys rl => RL.RowToList is rl => FamilyDef state is os m -> List String
inputsFromDef _ = Record.keys (Proxy :: Proxy is)


{- Maps / Folds tags -}


data MapFamilyDefs x = MapFamilyDefs
data MapFamilyDefsIndexed x = MapFamilyDefsIndexed

data FoldFamilyDefs x = FoldFamilyDefs

data FoldFamilyDefsIndexed x = FoldFamilyDefsIndexed


{- Map classes and instances -}


instance mappingTo ::
  ( ConvertFamilyDefTo x ) =>
  HM.Mapping (MapFamilyDefs x) (FamilyDef state is os m) x where
  mapping MapFamilyDefs = convertFamilyDef


instance mappingIndexedTo ::
  ( IsSymbol f, ConvertFamilyDefIndexedTo x ) =>
  HM.MappingWithIndex (MapFamilyDefsIndexed x) (Proxy f) (FamilyDef state is os m) x where
  mappingWithIndex MapFamilyDefsIndexed psym = convertFamilyDefIndexed $ familyP psym


class Map :: RL.RowList Type -> Row Type -> Type -> Row Type -> Constraint
class
    ( RL.RowToList families rli
    , ConvertFamilyDefTo x
    , HM.MapRecordWithIndex rli (HM.ConstMapping (MapFamilyDefs x)) families result
    ) <= Map rli families x result

instance mapFamilies_ ::
    ( RL.RowToList families rli
    , ConvertFamilyDefTo x
    , HM.MapRecordWithIndex rli (HM.ConstMapping (MapFamilyDefs x)) families result
    ) => Map rli families x result


class MapI :: RL.RowList Type -> Row Type -> Type -> Row Type -> Constraint
class
    ( RL.RowToList families rli
    , ConvertFamilyDefIndexedTo x
    , HM.MapRecordWithIndex rli (MapFamilyDefsIndexed x) families result
    ) <= MapI rli families x result

instance mapFamiliesIndexed_ ::
    ( RL.RowToList families rli
    , ConvertFamilyDefIndexedTo x
    , HM.MapRecordWithIndex rli (MapFamilyDefsIndexed x) families result
    ) => MapI rli families x result


class
    ( Monoid (ff result)
    , ConvertFamilyDefTo result
    , RL.RowToList families rl
    , HF.FoldlRecord (HF.ConstFolding (FoldFamilyDefs result)) (ff result) rl families (ff result)
    ) <= Fold rl ff result families


{- Folds classes and instances -}


instance fold ::
    ( Monoid (ff result)
    , ConvertFamilyDefTo result
    , RL.RowToList families rl
    , HF.FoldlRecord (HF.ConstFolding (FoldFamilyDefs result)) (ff result) rl families (ff result)
    ) => Fold rl ff result families

class
    ( Monoid (ff result)
    , ConvertFamilyDefIndexedTo result
    , RL.RowToList families rl
    , HF.FoldlRecord (FoldFamilyDefsIndexed result) (ff result) rl families (ff result)
    ) <= FoldI rl ff result families

instance foldI ::
    ( Monoid (ff result)
    , ConvertFamilyDefIndexedTo result
    , RL.RowToList families rl
    , HF.FoldlRecord (FoldFamilyDefsIndexed result) (ff result) rl families (ff result)
    ) => FoldI rl ff result families


instance foldDefsArr ::
    ( IsSymbol f, ConvertFamilyDefTo x )
    => HF.Folding
            (FoldFamilyDefs x)
            (Array x)
            (FamilyDef state is os m)
            (Array x)
    where
    folding FoldFamilyDefs acc def = convertFamilyDef def : acc


instance foldDefsIndexedArr ::
    ( IsSymbol f, ConvertFamilyDefIndexedTo x )
    => HF.FoldingWithIndex
            (FoldFamilyDefsIndexed x)
            (Proxy f)
            (Array x)
            (FamilyDef state is os m)
            (Array x)
    where
    foldingWithIndex FoldFamilyDefsIndexed sym acc def = convertFamilyDefIndexed (familyP sym) def : acc


{- Converters -}


class ConvertFamilyDefTo x where
    convertFamilyDef :: forall state is os m. FamilyDef state is os m -> x


class ConvertFamilyDefIndexedTo x where
    convertFamilyDefIndexed :: forall f state is os m. IsSymbol f => Family' f -> FamilyDef state is os m -> x




{- Implementations -}


hmap
    :: forall families families' rl x
     . Map rl families x families'
    => Record families
    -> Record families'
hmap = HM.hmap (MapFamilyDefs :: MapFamilyDefs x)


hmapWithIndex
    :: forall families families' rl x
     . MapI rl families x families'
    => Record families
    -> Record families'
hmapWithIndex =
    HM.hmapWithIndex (MapFamilyDefsIndexed :: MapFamilyDefsIndexed x)


hfoldl
    :: forall families x rl
     . Fold rl Array x families
    => Record families
    -> Array x
hfoldl =
    HF.hfoldl (FoldFamilyDefs :: FoldFamilyDefs x) ([] :: Array x)


hfoldlWithIndex
    :: forall families x rl
     . FoldI rl Array x families
    => Record families
    -> Array x
hfoldlWithIndex =
    HF.hfoldlWithIndex (FoldFamilyDefsIndexed :: FoldFamilyDefsIndexed x) ([] :: Array x)