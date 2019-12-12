module Rpd.Test.Util.Actions
    ( getOrFail
    , failIfNoErrors
    , failWith
    ) where


import Prelude
import Data.String (joinWith)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Aff (Aff)

import Test.Spec.Assertions (fail)


-- TODO: some core function?
getOrFail
  :: forall err x
   . Show err
  => Array err /\ x
  -> x
  -> Aff x
getOrFail ([] /\ v) _ = pure v
getOrFail (errors /\ _) default =
  failWith errors >>= (const $ pure default)


failIfNoErrors
  :: forall err x
   . String
  -> Array err /\ x
  -> Aff Unit
failIfNoErrors message ([] /\ _) = fail message
failIfNoErrors _ _ = pure unit


failWith :: forall err. Show err => Array err -> Aff Unit
failWith errors = fail $ joinWith "," $ show <$> errors
