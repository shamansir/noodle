module Rpd.Test.Util.Actions
    ( getOrFail
    , getOrFail'
    , failIfNoError
    , failIfNoErrors
    , failWith
    ) where


import Prelude
import Data.Tuple.Nested (type (/\), (/\))
import Data.String (joinWith)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)

import Data.Covered (Covered(..))

import Test.Spec.Assertions (fail)


getOrFail
  :: forall err x
   . Show err
  => Covered err x
  -> Aff x
getOrFail (Recovered err x) =
  (fail $ show err) >>= (const $ pure x)
getOrFail (Carried x) =
  pure x


getOrFail'
  :: forall err x
   . Show err
  => Array err /\ x
  -> x
  -> Aff x
getOrFail' ([] /\ v) _ = pure v
getOrFail' (errors /\ _) default =
  failWith errors >>= (const $ pure default)


getOrFail''
  :: forall err x
   . Show err
  => err
  -> x
  -> Maybe x
  -> Aff x
getOrFail'' err x Nothing =
  (fail $ show err) >>= (const $ pure x)
getOrFail'' _ _ (Just x) =
  pure x


failIfNoError
  :: forall err x
   . String
  -> Covered err x
  -> Aff Unit
failIfNoError message (Carried _) = fail message
failIfNoError _ _ = pure unit


failIfNoErrors
  :: forall err x
   . String
  -> Array err /\ x
  -> Aff Unit
failIfNoErrors message ([] /\ _) = fail message
failIfNoErrors _ _ = pure unit


failWith :: forall err. Show err => Array err -> Aff Unit
failWith errors = fail $ joinWith "," $ show <$> errors
