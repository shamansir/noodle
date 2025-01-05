module Noodle.Fn.Updates
    ( module GenericUpdates
    , Update, MergedUpdate
    , toFn
    )
    where

import Noodle.Id as Id
import Noodle.Fn.ToFn (Fn)

import Noodle.Fn.Generic.Updates (InletsUpdate(..), OutletsUpdate(..), UpdateFocus(..), toRecord, fromRecord) as GenericUpdates


import Noodle.Raw.Fn.Updates as Raw

type Update state (is :: Row Type) (os :: Row Type) repr = Raw.Update state repr
type MergedUpdate state (is :: Row Type) (os :: Row Type) repr = Raw.MergedUpdate state repr


toFn :: forall state is os repr. Id.NodeR -> MergedUpdate state is os repr -> Fn repr repr
toFn = Raw.toFn
