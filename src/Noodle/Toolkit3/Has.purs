module Noodle.Toolkit3.Has where


import Type.Data.Symbol (class IsSymbol)
import Prim.Row as Row


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


-- TODO: Move `ListsFamilies` here