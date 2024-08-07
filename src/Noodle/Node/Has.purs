module Noodle.Node.Has where

import Prelude

import Prim.Row (class Cons) as Row
import Data.Symbol (class IsSymbol)


class HasInlet :: forall k. Row k -> Symbol -> k -> Constraint
class (IsSymbol i, Row.Cons i din is is) <= HasInlet is i din
instance (IsSymbol i, Row.Cons i din is is) => HasInlet is i din


class HasOutlet :: forall k. Row k -> Symbol -> k -> Constraint
class (IsSymbol o, Row.Cons o dout os os) <= HasOutlet os o dout
instance (IsSymbol o, Row.Cons o dout os os) => HasOutlet os o dout