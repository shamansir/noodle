module Noodle.Fn.Updates
    ( module GenericUpdates
    , Update, MergedUpdate
    )
    where


import Noodle.Fn.Generic.Updates (InletsUpdate(..), OutletsUpdate(..), UpdateFocus(..), toTuple) as GenericUpdates


import Noodle.Fn.Raw.Updates as Raw

type Update state (is :: Row Type) (os :: Row Type) repr = Raw.Update state repr
type MergedUpdate state (is :: Row Type) (os :: Row Type) repr = Raw.MergedUpdate state repr
