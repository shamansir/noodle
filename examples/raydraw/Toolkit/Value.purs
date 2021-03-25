module RayDraw.Toolkit.Value where


import Prelude

data Value
    = Bang


data Aggregate
    = All
    | Exactly Int


instance showValue :: Show Value where
    show Bang = "◌"
