module Data.Layout.Ordered where


import App.Style.Order (Order)


-- TODO: is layout instance

data Item a
    = Horz (Order (Item a))
    | Vert (Order (Item a))


data Ordered a = Ordered (Order (Item a))