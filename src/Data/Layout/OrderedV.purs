module Data.Layout.OrderedV where


import App.Style.Order (Order)

import Data.Tuple.Nested ((/\), type (/\))


-- TODO: is layout instance

data Horz s a = Horz (Order a)


data Vert s a = Vert (Order (Horz s a))


type Ordered a = Vert a