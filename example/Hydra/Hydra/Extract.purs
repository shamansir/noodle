module Hydra.Extract where


import Prelude ((<$>), ($))

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (catMaybes)

import Hydra


numV :: Value -> Maybe Number
numV (Num n) = Just n
numV _ = Nothing


numOr :: Number -> Hydra -> Number
numOr def (Value val) = fromMaybe def $ numV val
numOr def _ = def


seq :: Hydra -> Array Value
seq (Value (Seq xs)) = xs
seq (Value v) = [ v ]
seq _ = []


seq' :: Hydra -> Array Number
seq' (Value (Num n)) = [ n ]
seq' (Value (Seq xs)) = catMaybes $ numV <$> xs
seq' _ = []