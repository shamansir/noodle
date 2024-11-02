module Noodle.Fn.Raw.Updates where

import Noodle.Fn.Generic.Updates as Generic

import Data.Map (Map)
import Noodle.Id (InputR, OutputR)


type PreUpdatesRow state repr = Generic.PreUpdatesRow state (Map InputR repr) (Map OutputR repr)
type PostUpdatesRow state repr = Generic.PostUpdatesRow state (Map InputR repr) (Map OutputR repr)
type FocusedUpdate state repr = Generic.FocusedUpdate state (Map InputR repr) (Map OutputR repr)