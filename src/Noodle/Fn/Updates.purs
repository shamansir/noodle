module Noodle.Fn.Updates
    ( module GenericUpdates
    , Update, MergedUpdate
    , toSignature
    )
    where

import Noodle.Id as Id
import Noodle.Fn.Signature (Signature)

import Noodle.Fn.Generic.Updates (InletsUpdate(..), OutletsUpdate(..), UpdateFocus(..), toRecord, fromRecord) as GenericUpdates

import Noodle.Repr.ValueInChannel (ValueInChannel)


import Noodle.Raw.Fn.Updates as Raw

type Update state (is :: Row Type) (os :: Row Type) repr = Raw.Update state repr
type MergedUpdate state (is :: Row Type) (os :: Row Type) repr = Raw.MergedUpdate state repr


toSignature :: forall state is os repr. Id.NodeR -> MergedUpdate state is os repr -> Signature (ValueInChannel repr) (ValueInChannel repr)
toSignature = Raw.toSignature
