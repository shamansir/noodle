module Noodle.Toolkit3.MapsFolds.Repr where

import Prelude

import Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy)
import Data.Tuple.Nested ((/\), type (/\))

import Heterogeneous.Mapping as HM

import Noodle.Id (Family', familyP, inputP, outputP)
import Noodle.Family.Def as Family
import Noodle.Toolkit3.Path (Path(..))
import Noodle.Id (class HasInputsAt, class HasOutputsAt) as Fn



data ToReprTop :: forall k. k -> Type
data ToReprTop repr = ToReprTop (Repr repr)
data ToReprDownI :: forall k. Symbol -> k -> Type
data ToReprDownI f repr = ToReprDownI (Family' f) (Repr repr)
data ToReprDownO :: forall k. Symbol -> k -> Type
data ToReprDownO f repr = ToReprDownO (Family' f) (Repr repr)


data Repr :: forall k. k -> Type
data Repr a = Repr



class HasRepr a repr where
    toRepr :: forall f i o. Path f i o -> a -> repr -- include Repr as kind here?


instance toReprTopInstance ::
    ToReprHelper sym is iks os oks repr_is repr_os repr state =>
    HM.MappingWithIndex
        (ToReprTop repr)
        (Proxy sym)
        (Family.Def state is os m)
        (repr /\ Record repr_is /\ Record repr_os)
    where
    mappingWithIndex (ToReprTop repr) sym (Family.Def (s /\ iRec /\ oRec /\ _)) =
        toRepr (FamilyP $ familyP sym) s
            /\ HM.hmapWithIndex (ToReprDownI (familyP sym) repr) iRec
            /\ HM.hmapWithIndex (ToReprDownO (familyP sym) repr) oRec


instance toReprDownIInstance ::
    ( IsSymbol sym
    , HasRepr a repr
    ) =>
    HM.MappingWithIndex
        (ToReprDownI family repr)
        (Proxy sym)
        a
        repr -- (FromRepr repr)
    where
    mappingWithIndex (ToReprDownI family _) sym = toRepr (InputP family $ inputP sym)


instance toReprDownOInstance ::
    ( IsSymbol sym
    , HasRepr a repr
    ) =>
    HM.MappingWithIndex
        (ToReprDownO family repr)
        (Proxy sym)
        a
        repr -- (FromRepr repr)
    where
    mappingWithIndex (ToReprDownO family _) sym = toRepr (OutputP family $ outputP sym)



class
    ( IsSymbol sym
    , HasRepr state repr
    , HM.MapRecordWithIndex iks (ToReprDownI sym repr) is repr_is
    , HM.MapRecordWithIndex oks (ToReprDownO sym repr) os repr_os
    , Fn.HasInputsAt is iks
    , Fn.HasOutputsAt os oks
    ) <= ToReprHelper sym is iks os oks repr_is repr_os repr state
instance
    ( IsSymbol sym
    , HasRepr state repr
    , HM.MapRecordWithIndex iks (ToReprDownI sym repr) is repr_is
    , HM.MapRecordWithIndex oks (ToReprDownO sym repr) os repr_os
    , Fn.HasInputsAt is iks
    , Fn.HasOutputsAt os oks
    ) => ToReprHelper sym is iks os oks repr_is repr_os repr state