module App.Layout where


import Data.BinPack.R2 (Bin2)

import Noodle.Node (Node)


cellSize = 30


type Layout d a = Bin2 Int (Node d a)