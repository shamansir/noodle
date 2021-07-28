module Hydra where


import Prelude (($), (<<<))
import Data.Maybe (Maybe(..))


data Value
    = Num Number
    | Mouse
    | Time
    | Seq (Array Number)
    -- Harmonic Int
    -- FN (Time -> Value)


data Hydra
    = None
    | Osc Value Value Value
    | Value' Value
    | Join (Array Hydra)
    | Out (Maybe Int) Hydra


default :: Hydra
default = None


defaultOsc :: Hydra
defaultOsc = osc 60.0 0.1 0.0


osc :: Number -> Number -> Number -> Hydra
osc frequency syn offset =
    Osc (Num frequency) (Num syn) (Num offset)


tryOsc :: Hydra -> Hydra -> Hydra -> Hydra
tryOsc (Value' freq) (Value' sync) (Value' offset) =
    Osc freq sync offset
tryOsc _ _ _ = None


num :: Number -> Hydra
num = Value' <<< Num


out :: Int -> Hydra -> Hydra
out = Out <<< Just


out' :: Hydra -> Hydra
out' = Out $ Nothing
