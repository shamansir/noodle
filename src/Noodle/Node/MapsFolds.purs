module Noodle.Node.MapsFolds where

import Type.Data.Symbol (class IsSymbol)

import Noodle.Id (class HasInputsAt, class HasOutputsAt, Family')
import Noodle.Node (Node)

import Unsafe.Coerce (unsafeCoerce)


--class ConvertNodeTo' :: RL.RowList -> Row Type -> Row Type -> Type -> Constraint
class (HasInputsAt is irl, HasOutputsAt os orl) <= ConvertNodeTo' (is :: Row Type) (os :: Row Type) irl orl repr x | is -> irl, os -> orl where
    convertNode' :: forall f state m. Node f state is os repr m -> x


-- class ConvertNodeTo (focus :: Focus) x where
class ConvertNodeTo x where
    convertNode :: forall f state is os repr m. Node f state is os repr m -> x


class ConvertNodeIndexedTo x where
    convertNodeIndexed :: forall f state is os repr m. IsSymbol f => Family' f -> Int -> Node f state is os repr m -> x


instance convertToItself :: ConvertNodeTo (Node f' state' is' os' repr' m') where
    convertNode :: forall f state is os repr m. Node f state is os repr m -> Node f' state' is' os' repr' m'
    -- convertNode :: Node f' state' is' os' m' -> Node f' state' is' os' m'
    convertNode = unsafeCoerce
    -- convertNode = identity


instance convertIndexedToItself :: ConvertNodeIndexedTo (Node f' state' is' os' repr' m') where
    convertNodeIndexed
        :: forall f state is os repr m
         . IsSymbol f
        => Family' f
        -> Int
        -> Node f state is os repr m
        -> Node f' state' is' os' repr' m'
    convertNodeIndexed _ _ = unsafeCoerce
