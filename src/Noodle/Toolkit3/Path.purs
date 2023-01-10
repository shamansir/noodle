module Noodle.Toolkit3.Path where

import Noodle.Id (Family', Input', Output')


data Path f i o
    = FamilyP (Family' f)
    | InputP (Family' f) (Input' i)
    | OutputP (Family' f) (Output' o)