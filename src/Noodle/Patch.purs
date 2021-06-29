module Noodle.Patch where


import Noodle.Node

import Data.Map.Extra (type (/->))


data Patch d a = Patch String (String /-> Node d a)
