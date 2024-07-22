module Noodle.Fn.Updates where


import Noodle.Fn.Raw.Updates as Raw

type PreUpdatesRow  state (is :: Row Type) (os :: Row Type) repr = Raw.PreUpdatesRow  state repr
type PostUpdatesRow state (is :: Row Type) (os :: Row Type) repr = Raw.PostUpdatesRow state repr
type FocusedUpdate  state (is :: Row Type) (os :: Row Type) repr = Raw.FocusedUpdate  state repr
