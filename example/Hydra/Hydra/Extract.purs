module Hydra.Extract where


import Hydra


numOr :: Number -> Hydra -> Number
numOr _ (Value (Num n)) = n
numOr def _ = def


seq :: Hydra -> Array Number
seq (Value (Num n)) = [ n ]
seq (Value (Seq xs)) = xs
seq _ = []