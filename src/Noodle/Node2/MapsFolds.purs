module Noodle.Node2.MapsFolds where

import Prelude

import Type.Data.Symbol (class IsSymbol)

import Data.UniqueHash (UniqueHash)
import Noodle.Id (Family', NodeId, familyP, InputR, class HasInputsAt, class HasOutputsAt)
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Node2.MapsFolds.Repr (Repr, ToReprTop(..), NodeLineRec, NodeLineMap)

import Unsafe.Coerce (unsafeCoerce)


--class ConvertNodeTo' :: RL.RowList -> Row Type -> Row Type -> Type -> Constraint
class (HasInputsAt is irl, HasOutputsAt os orl) <= ConvertNodeTo' (is :: Row Type) (os :: Row Type) irl orl x | is -> irl, os -> orl where
    convertNode' :: forall f state m. Node f state is os m -> x


-- class ConvertNodeTo (focus :: Focus) x where
class ConvertNodeTo x where
    convertNode :: forall f state is os m. Node f state is os m -> x


class ConvertNodeIndexedTo x where
    convertNodeIndexed :: forall f state is os m. IsSymbol f => Family' f -> Int -> Node f state is os m -> x


instance convertToItself :: ConvertNodeTo (Node f' state' is' os' m') where
    convertNode :: forall f state is os m. Node f state is os m -> Node f' state' is' os' m'
    -- convertNode :: Node f' state' is' os' m' -> Node f' state' is' os' m'
    convertNode = unsafeCoerce
    -- convertNode = identity


instance convertIndexedToItself :: ConvertNodeIndexedTo (Node f' state' is' os' m') where
    convertNodeIndexed
        :: forall f state is os m
         . IsSymbol f
        => Family' f
        -> Int
        -> Node f state is os m
        -> Node f' state' is' os' m'
    convertNodeIndexed _ _ = unsafeCoerce
