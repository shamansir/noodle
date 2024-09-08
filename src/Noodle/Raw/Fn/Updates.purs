module Noodle.Raw.Fn.Updates where

import Noodle.Fn.Generic.Updates as Generic

import Data.Map (Map)
import Noodle.Id (InletR, OutletR)


type Update state repr = Generic.Update state (Map InletR repr) (Map OutletR repr)
type MergedUpdate state repr = Generic.MergedUpdate state (Map InletR repr) (Map OutletR repr)