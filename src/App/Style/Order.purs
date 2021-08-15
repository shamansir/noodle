module App.Style.Order where


import Prelude ((<<<), ($), (+))

import Data.Set.Ordered (OSet)
import Data.Set.Ordered as OSet
import Data.Foldable (class Foldable, foldr)


data Order a = Order (OSet a)


fromFoldable :: forall f a. Foldable f => f a -> Order a
fromFoldable = Order <<< OSet.fromFoldable

-- TODO: IsLayout instance


sizeBy :: forall a. (a -> Number) -> Order a -> Number
sizeBy sizeF (Order set) = foldr (+) 0.0 $ OSet.map sizeF set