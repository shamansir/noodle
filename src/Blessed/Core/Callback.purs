module Blessed.Core.Callback where

import Prelude

import Effect (Effect)
import Blessed.Internal.BlessedOp (BlessedOp)


newtype Callback state = Callback (BlessedOp state Effect)


-- TODO
-- in any method which is BlessedOp m, it is (or should be) possible to register callback, generate some id to it and the call,
-- or just pass it to the Blessed core