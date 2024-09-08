module Noodle.Rec.Fn.Updates where

import Noodle.Fn.Generic.Updates as Generic


type Update state is os = Generic.Update state (Record is) (Record os)
type MergedUpdate state is os = Generic.MergedUpdate state (Record is) (Record os)
