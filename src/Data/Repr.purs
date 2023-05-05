module Data.Repr
    ( Repr
    , class ToRepr, toRepr
    , class FromRepr, fromRepr
    , unwrap
    )
    where

import Data.Maybe (Maybe)


-- FIXME: Merge with `Node2.MapsFolds.Repr` and `Patch4.MapsFolds.Repr` and `Toolkit3.MapsFolds.Repr`.
-- FIXME: Maybe reorganize into `MapsFolds.Node2.Path` & `MapsFolds.Node2.Repr` and `MapsFolds.Patch4` and `MapsFolds.Toolkit3.Path` & `MapsFolds.Toolkit3.Repr`

data Repr a = Repr a


class ToRepr a repr where
    toRepr :: a -> Maybe (Repr repr)


class FromRepr repr a where
    fromRepr :: Repr repr -> Maybe a


unwrap :: forall a. Repr a -> a
unwrap (Repr a) = a