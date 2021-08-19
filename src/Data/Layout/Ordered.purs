module Data.Layout.OrderedH where


import App.Style.Order (Order)

import Data.Tuple.Nested ((/\), type (/\))


-- TODO: `IsLayout` instance (AutoSizedLayout?)
-- TODO: Replace `Order` with just Ordered Ser

data Size
    = Auto
    | Fixed Number
    | Percentage Number
    | Cells Number


data HBox s a = Horz (Order (s /\ VBox s a))


data VBox s a = Vert (Order (s /\ a))


type Ordered s a = HBox s a