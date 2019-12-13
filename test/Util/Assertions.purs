module Rpd.Test.Util.Assertions where

import Prelude
import Data.Maybe
import Effect.Aff (Aff())

import Test.Spec.Assertions


shouldHaveValue :: forall t. Show t => Maybe t -> Aff Unit
shouldHaveValue v =
  when (not $ isJust v) $
    fail $ show v <> " contains no value"
