module Noodle.Toolkit
    where

import Data.Map.Extra (type (/->))


import Noodle.Node (Node)


data Toolkit d a = Toolkit (String /-> Node d a)


data Renderer d a view = Renderer (String -> Node d a -> view)