module Data.Layout.OrderedH where


import App.Style.Order (Order)

import Data.Tuple.Nested ((/\), type (/\))


-- TODO: is layout instance

data Horz s a = Horz (Order (Vert s a))


data Vert s a = Vert (Order (s /\ a))


type Ordered a = Horz a