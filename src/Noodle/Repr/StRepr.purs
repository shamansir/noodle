module Noodle.Repr.StRepr where

import Prelude



-- https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html
-- import Data.Generic.Rep as GR


class StRepr repr a | repr -> a where
  to :: a -> repr
  from :: repr -> a