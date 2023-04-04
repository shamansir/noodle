module Noodle.Toolkit3.Has where


import Type.Data.Symbol (class IsSymbol)
import Prim.Row as Row

import Noodle.Patch4.Has (class HasInstancesOf)


class HasFamilyDef :: forall k. Symbol -> Row k -> Row k -> k -> Constraint
class
    ( IsSymbol f
    , Row.Cons f x families' families
    )
    <= HasFamilyDef f families' families x -- FIXME: use newtype
instance
    ( IsSymbol f
    , Row.Cons f x families' families
    )
    => HasFamilyDef f families' families x -- FIXME: use newtype


class HasFamilyDef' :: forall k. Symbol -> Row k -> Row k -> k -> Constraint
class
    ( Row.Cons f x families' families
    )
    <= HasFamilyDef' f families' families x -- FIXME: use newtype
instance
    ( Row.Cons f x families' families
    )
    => HasFamilyDef' f families' families x -- FIXME: use newtype


-- class ( IsSymbol f
--         , HasFamilyDef f fs (Families m) (Family.Def state is os m)
--         , HasInstancesOf f iis (Instances m) (Array (Noodle.Node f state is os m))
--         ) <= HasNodesOf f state fs iis is os m

-- instance ( IsSymbol f
--         , HasFamilyDef f fs (Families m) (Family.Def state is os m)
--         , HasInstancesOf f iis (Instances m) (Array (Noodle.Node f state is os m))
--         ) => HasNodesOf f state fs iis is os m


-- TODO: Move `ListsFamilies` here