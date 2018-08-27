module Rpd.Log
    ( reportError
    , reportAndReturn
    , runRpdLogging
    , extract
    ) where


import Prelude

import Rpd as R
import Effect (Effect)
import Effect.Class.Console (log)

reportError :: R.RpdError -> Effect Unit
reportError = log <<< (<>) "RPD Error: " <<< show


reportAndReturn :: forall a. a -> R.RpdError -> Effect a
reportAndReturn v err =
  reportError err >>= \_ -> pure v


runRpdLogging :: forall a. (a -> Effect Unit) -> R.Rpd a -> Effect Unit
runRpdLogging onSuccess rpd =
  R.run' reportError onSuccess rpd


extract :: forall a. a -> R.Rpd a -> Effect a
extract def rpd =
  R.run' (reportAndReturn def) pure rpd
