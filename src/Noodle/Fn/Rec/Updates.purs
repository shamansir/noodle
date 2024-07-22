module Noodle.Fn.Rec.Updates where

import Noodle.Fn.Generic.Updates as Generic


type PreUpdatesRow  state is os = Generic.PreUpdatesRow  state (Record is) (Record os)
type PostUpdatesRow state is os = Generic.PostUpdatesRow state (Record is) (Record os)
type FocusedUpdate  state is os = Generic.FocusedUpdate  state (Record is) (Record os)
