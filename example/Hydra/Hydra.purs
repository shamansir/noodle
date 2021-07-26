module Hydra where


import Data.Maybe (Maybe)


data Value
    = Num Number
    | Mouse
    | Time
    | Seq (Array Number)
    -- Harmonic Int


data Hydra
    = None
    | Osc Value Value Value
    | Value' Value
    | Join (Array Hydra)
    | Out Hydra (Maybe Int)


default :: Hydra
default = None