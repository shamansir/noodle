module Noodle.Toolkit3.MapsFolds where


import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.Array ((:))
import Data.Array as Array
import Data.Symbol (SProxy)
import Data.List (List)
import Data.Bifunctor (bimap)
import Data.Maybe (Maybe(..))

import Type.Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(..))


import Record.Extra (class Keys)
import Record.Extra as Record
import Prim.RowList as RL

import Heterogeneous.Mapping as HM
import Heterogeneous.Folding as HF

import Noodle.Id (Family', familyP)
import Noodle.Id (InputR, OutputR, keysToInputsR, keysToOutputsR) as Fn
import Noodle.Id (class HasInputsAt, class HasOutputsAt) as Fn
import Noodle.Id (reflectInputR, reflectOutputR) as Fn
import Noodle.Id (class ListsFamilies)
import Noodle.Fn2 (Fn)
import Noodle.Family.Def as Family


{- Maps / Folds tags -}


data MapFamilyDefs x = MapFamilyDefs
data MapFamilyDefsIndexed x = MapFamilyDefsIndexed

data FoldFamilyDefs x = FoldFamilyDefs

data FoldFamilyDefsIndexed x = FoldFamilyDefsIndexed


{- Map classes and instances -}


instance mappingTo ::
  ( ConvertFamilyDefTo x ) =>
  HM.Mapping (MapFamilyDefs x) (Family.Def state is os m) x where
  mapping MapFamilyDefs = convertFamilyDef


instance mappingIndexedTo ::
  ( IsSymbol f, ConvertFamilyDefIndexedTo x ) =>
  HM.MappingWithIndex (MapFamilyDefsIndexed x) (Proxy f) (Family.Def state is os m) x where
  mappingWithIndex MapFamilyDefsIndexed psym = convertFamilyDefIndexed $ familyP psym


class
    ( RL.RowToList families rli
    , ConvertFamilyDefTo x
    , HM.MapRecordWithIndex rli (HM.ConstMapping (MapFamilyDefs x)) families target
    ) <= Map (rli :: RL.RowList Type) (families :: Row Type) (target :: Row Type) x

instance mapFamilies_ ::
    ( RL.RowToList families rli
    , ConvertFamilyDefTo x
    , HM.MapRecordWithIndex rli (HM.ConstMapping (MapFamilyDefs x)) families target
    ) => Map rli families target x


class
    ( RL.RowToList families rli
    , ConvertFamilyDefIndexedTo x
    , HM.MapRecordWithIndex rli (MapFamilyDefsIndexed x) families target
    ) <= MapI (rli :: RL.RowList Type) (families :: Row Type) (target :: Row Type) x

instance mapFamiliesIndexed_ ::
    ( RL.RowToList families rli
    , ConvertFamilyDefIndexedTo x
    , HM.MapRecordWithIndex rli (MapFamilyDefsIndexed x) families target
    ) => MapI rli families target x


{- Special Maps -}

data ToShape = ToShape


data ExtractShape = ExtractShape


instance toShapesMap ::
    ( Fn.HasInputsAt is iks
    , Fn.HasOutputsAt os oks
    ) =>
    HM.Mapping
        ToShape
        (Family.Def state is os m)
        (List Fn.InputR /\ List Fn.OutputR)
    where
    mapping ToShape _ =
        Fn.keysToInputsR (Proxy :: Proxy is)
        /\ Fn.keysToOutputsR (Proxy :: Proxy os)


instance extractShapeMap ::
    HM.Mapping
        ExtractShape
        (List Fn.InputR /\ List Fn.OutputR)
        (Array String /\ Array String)
    where
    mapping ExtractShape =
        bimap
            (map Fn.reflectInputR >>> Array.fromFoldable)
            (map Fn.reflectOutputR >>> Array.fromFoldable)



data ToReprTop :: forall k. k -> Type
data ToReprTop repr = ToReprTop (Repr repr)
data ToReprDown :: forall k. k -> Type
data ToReprDown repr = ToReprDown (Repr repr)


data Repr :: forall k. k -> Type
data Repr a = Repr

-- foreign import data NoSymbol :: forall k. k -> Repr k
-- foreign import data HasSymbol :: forall k. Symbol -> k -> Repr k

-- data ReprWithSym :: forall k. Symbol -> k -> Type
-- data ReprWithSym sym a = ReprWithSym


class HasRepr a repr where
    toRepr :: forall sym. IsSymbol sym => Proxy sym -> a -> repr -- include Repr as kind here?


instance toReprTopInstance ::
    ToReprHelper sym is iks os oks repr_is repr_os repr state =>
    HM.MappingWithIndex
        (ToReprTop repr)
        (Proxy sym)
        (Family.Def state is os m)
        (repr /\ Record repr_is /\ Record repr_os)
    where
    mappingWithIndex (ToReprTop repr) sym (Family.Def (s /\ iRec /\ oRec /\ _)) =
        toRepr sym s
            /\ HM.hmapWithIndex (ToReprDown repr) iRec
            /\ HM.hmapWithIndex (ToReprDown repr) oRec


instance toReprDownInstance ::
    ( IsSymbol sym
    , HasRepr a repr
    ) =>
    HM.MappingWithIndex
        (ToReprDown repr)
        (Proxy sym)
        a
        repr -- (FromRepr repr)
    where
    mappingWithIndex (ToReprDown _) = toRepr


{- Folds classes and instances -}

class
    ( Monoid (ff result)
    , ConvertFamilyDefTo result
    , RL.RowToList families rl
    , HF.FoldlRecord (HF.ConstFolding (FoldFamilyDefs result)) (ff result) rl families (ff result)
    ) <= Fold (rl :: RL.RowList Type) (families :: Row Type) (ff :: Type -> Type) result


instance fold ::
    ( Monoid (ff result)
    , ConvertFamilyDefTo result
    , RL.RowToList families rl
    , HF.FoldlRecord (HF.ConstFolding (FoldFamilyDefs result)) (ff result) rl families (ff result)
    ) => Fold rl families ff result

class
    ( Monoid (ff result)
    , ConvertFamilyDefIndexedTo result
    , RL.RowToList families rl
    , HF.FoldlRecord (FoldFamilyDefsIndexed result) (ff result) rl families (ff result)
    ) <= FoldI (rl :: RL.RowList Type) (families :: Row Type) (ff :: Type -> Type) result

instance foldI ::
    ( Monoid (ff result)
    , ConvertFamilyDefIndexedTo result
    , RL.RowToList families rl
    , HF.FoldlRecord (FoldFamilyDefsIndexed result) (ff result) rl families (ff result)
    ) => FoldI rl families ff result


instance foldDefsArr ::
    ( ConvertFamilyDefTo x )
    => HF.Folding
            (FoldFamilyDefs x)
            (Array x)
            (Family.Def state is os m)
            (Array x)
    where
    folding FoldFamilyDefs acc def = convertFamilyDef def : acc


instance foldDefsIndexedArr ::
    ( IsSymbol f, ConvertFamilyDefIndexedTo x )
    => HF.FoldingWithIndex
            (FoldFamilyDefsIndexed x)
            (Proxy f)
            (Array x)
            (Family.Def state is os m)
            (Array x)
    where
    foldingWithIndex FoldFamilyDefsIndexed sym acc def = convertFamilyDefIndexed (familyP sym) def : acc


{- Converters -}


class ConvertFamilyDefTo x where
    convertFamilyDef :: forall state is os m. Family.Def state is os m -> x


class ConvertFamilyDefIndexedTo x where
    convertFamilyDefIndexed :: forall f state is os m. IsSymbol f => Family' f -> Family.Def state is os m -> x




{- Implementations -}


hmap
    :: forall families families' rl x
     . Map rl families families' x
    => Proxy x
    -> Record families
    -> Record families'
hmap _ = HM.hmap (MapFamilyDefs :: MapFamilyDefs x)


hmapWithIndex
    :: forall families families' rl x
     . MapI rl families families' x
    => Proxy x
    -> Record families
    -> Record families'
hmapWithIndex _ =
    HM.hmapWithIndex (MapFamilyDefsIndexed :: MapFamilyDefsIndexed x)


hfoldl
    :: forall families x rl
     . Fold rl families Array x
    => Record families
    -> Array x
hfoldl =
    HF.hfoldl (FoldFamilyDefs :: FoldFamilyDefs x) ([] :: Array x)


hfoldlWithIndex
    :: forall families x rl
     . FoldI rl families Array x
    => Record families
    -> Array x
hfoldlWithIndex =
    HF.hfoldlWithIndex (FoldFamilyDefsIndexed :: FoldFamilyDefsIndexed x) ([] :: Array x)


{- Special : Implementations -}


class
    ( RL.RowToList families fs
    , HM.HMap ToShape (Record families) (Record shapes)
    , HM.MapRecordWithIndex fs (HM.ConstMapping ToShape) families shapes
    )
    <= MapToShapes (fs :: RL.RowList Type) (families :: Row Type) (shapes :: Row Type)
instance
    ( RL.RowToList families fs
    , HM.HMap ToShape (Record families) (Record shapes)
    , HM.MapRecordWithIndex fs (HM.ConstMapping ToShape) families shapes
    )
    => MapToShapes fs families shapes


class
    ( RL.RowToList shapes fs
    , HM.HMap ExtractShape (Record shapes) (Record ex_shapes)
    , HM.MapRecordWithIndex fs (HM.ConstMapping ExtractShape) shapes ex_shapes
    )
    <= ExtractShapes (fs :: RL.RowList Type) (shapes :: Row Type) (ex_shapes :: Row Type)
instance
    ( RL.RowToList shapes fs
    , HM.HMap ExtractShape (Record shapes) (Record ex_shapes)
    , HM.MapRecordWithIndex fs (HM.ConstMapping ExtractShape) shapes ex_shapes
    )
    => ExtractShapes fs shapes ex_shapes


class
    ( IsSymbol sym
    , HasRepr state repr
    , HM.MapRecordWithIndex iks (ToReprDown repr) is repr_is
    , HM.MapRecordWithIndex oks (ToReprDown repr) os repr_os
    , Fn.HasInputsAt is iks
    , Fn.HasOutputsAt os oks
    ) <= ToReprHelper sym is iks os oks repr_is repr_os repr state
instance
    ( IsSymbol sym
    , HasRepr state repr
    , HM.MapRecordWithIndex iks (ToReprDown repr) is repr_is
    , HM.MapRecordWithIndex oks (ToReprDown repr) os repr_os
    , Fn.HasInputsAt is iks
    , Fn.HasOutputsAt os oks
    ) => ToReprHelper sym is iks os oks repr_is repr_os repr state


class
    ( ListsFamilies families fs
    , HM.MapRecordWithIndex fs (ToReprTop repr) families reprs
    )
    <= ExtractReprs
        (fs :: RL.RowList Type) (families :: Row Type)
        (reprs :: Row Type) (repr :: Type)
instance
    ( ListsFamilies families fs
    , HM.MapRecordWithIndex fs (ToReprTop repr) families reprs
    )
    => ExtractReprs fs families reprs repr


toShapes
    :: forall families fs shapes
     . MapToShapes fs families shapes
    => Record families
    -> Record shapes
toShapes = HM.hmap ToShape


extractShapes
    :: forall fs shapes ex_shapes
     . ExtractShapes fs shapes ex_shapes
    => Record shapes
    -> Record ex_shapes
extractShapes = HM.hmap ExtractShape


toReprs
    :: forall fs families reprs repr
     . ExtractReprs fs families reprs repr
    => Repr repr
    -> Record families
    -> Record reprs
toReprs repr = HM.hmapWithIndex (ToReprTop repr)