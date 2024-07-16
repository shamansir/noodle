module Noodle.Patch.Has where


import Type.Data.Symbol (class IsSymbol)
import Prim.Row as Row

import Noodle.Node (Node)


class HasInstancesOf :: forall k. Symbol -> Row k -> Row k -> k -> Constraint
class
    ( IsSymbol f
    , Row.Cons f x instances' instances
    )
    <= HasInstancesOf f instances' instances x -- FIXME: use newtype

instance
    ( IsSymbol f
    , Row.Cons f x instances' instances
    )
    => HasInstancesOf f instances' instances x -- FIXME: use newtype


class
    ( HasInstancesOf f instances' instances (Array (Node f state is os repr m))
    )
    <= HasNodesOf instances' instances f state is os repr m


instance
    ( HasInstancesOf f instances' instances (Array (Node f state is os repr m))
    )
    => HasNodesOf instances' instances f state is os repr m