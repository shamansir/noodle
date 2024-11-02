module Noodle.Toolkit.Path where

import Noodle.Id (Family', Input', Output')


data InToolkit f i o
    = FamilyP (Family' f)
    | InputP (Family' f) (Input' i)
    | OutputP (Family' f) (Output' o)