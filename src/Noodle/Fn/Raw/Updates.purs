module Noodle.Fn.Raw.Updates where

import Noodle.Fn.Generic.Updates as Generic

import Data.Map (Map)
import Noodle.Id (InletR, OutletR)


type PreUpdatesRow state repr = Generic.PreUpdatesRow state (Map InletR repr) (Map OutletR repr)
type PostUpdatesRow state repr = Generic.PostUpdatesRow state (Map InletR repr) (Map OutletR repr)
type FocusedUpdate state repr = Generic.FocusedUpdate state (Map InletR repr) (Map OutletR repr)