module Noodle.Toolkit.MapsFolds.Repr
    ( Repr(..)
    , ToReprTop(..)
    , ToReprDownI, ToReprDownO
    , class HasRepr
    , toRepr
    , class ToReprHelper
    , class ExtractReprs
    )
    where

import Prelude

import Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy)
import Data.Tuple.Nested ((/\), type (/\))
import Prim.RowList as RL
import Data.SOrder (SOrder)

import Heterogeneous.Mapping as HM

import Noodle.Id (Family', familyP, inputP', outputP', class ListsFamilies)
import Noodle.Family.Def as Family
import Noodle.Toolkit.Path (InToolkit(..))
import Noodle.Fn (inputsOrder, outputsOrder) as Fn
import Noodle.Id (class HasInputsAt, class HasOutputsAt) as Fn



data ToReprTop :: forall k. k -> Type
data ToReprTop repr = ToReprTop (Repr repr)
data ToReprDownI :: forall k. Symbol -> k -> Type
data ToReprDownI f repr = ToReprDownI (Family' f) SOrder (Repr repr)
data ToReprDownO :: forall k. Symbol -> k -> Type
data ToReprDownO f repr = ToReprDownO (Family' f) SOrder (Repr repr)


data Repr :: forall k. k -> Type
data Repr a = Repr



class HasRepr a repr where
    toRepr :: forall f i o. InToolkit f i o -> a -> repr -- include Repr as kind here?


instance toReprTopInstance ::
    ToReprHelper f is iks os oks repr_is repr_os repr state =>
    HM.MappingWithIndex
        (ToReprTop repr)
        (Proxy f)
        (Family.Def state is os repr m)
        (repr /\ Record repr_is /\ Record repr_os)
    where
    mappingWithIndex (ToReprTop repr) fsym (Family.Def (s /\ iRec /\ oRec /\ fn)) =
        toRepr (FamilyP $ familyP fsym) s
            /\ HM.hmapWithIndex (ToReprDownI (familyP fsym) (Fn.inputsOrder fn) repr) iRec
            /\ HM.hmapWithIndex (ToReprDownO (familyP fsym) (Fn.outputsOrder fn) repr) oRec


instance toReprDownIInstance ::
    ( IsSymbol i
    , HasRepr a repr
    ) =>
    HM.MappingWithIndex
        (ToReprDownI family repr)
        (Proxy i)
        a
        repr -- (FromRepr repr)
    where
    mappingWithIndex (ToReprDownI family iorder _) isym = toRepr (InputP family $ inputP' iorder isym)


instance toReprDownOInstance ::
    ( IsSymbol o
    , HasRepr a repr
    ) =>
    HM.MappingWithIndex
        (ToReprDownO family repr)
        (Proxy o)
        a
        repr -- (FromRepr repr)
    where
    mappingWithIndex (ToReprDownO family oorder _) osym = toRepr (OutputP family $ outputP' oorder osym)



class
    ( IsSymbol f
    , HasRepr state repr
    , Fn.HasInputsAt is iks
    , Fn.HasOutputsAt os oks
    , HM.MapRecordWithIndex iks (ToReprDownI f repr) is repr_is
    , HM.MapRecordWithIndex oks (ToReprDownO f repr) os repr_os
    ) <= ToReprHelper f is iks os oks repr_is repr_os repr state
instance
    ( IsSymbol f
    , HasRepr state repr
    , Fn.HasInputsAt is iks
    , Fn.HasOutputsAt os oks
    , HM.MapRecordWithIndex iks (ToReprDownI f repr) is repr_is
    , HM.MapRecordWithIndex oks (ToReprDownO f repr) os repr_os
    ) => ToReprHelper f is iks os oks repr_is repr_os repr state


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
