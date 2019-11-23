module Rpd.Test.Util.Either
    ( getOrFail
    , failIfNoError
    ) where


import Prelude
import Data.Either (Either(..))
import Effect.Aff (Aff)

import Test.Spec.Assertions (fail)


-- TODO: some core function?
getOrFail
  :: forall err x
   . Show err
  => Either err x
  -> x
  -> Aff x
getOrFail (Left err) default =
  (fail $ show err) >>= (const $ pure default)
getOrFail (Right v) _ = pure v


failIfNoError
  :: forall err x
   . String
  -> Either err x
  -> Aff Unit
failIfNoError _ (Left _) = pure unit
failIfNoError message (Right _) = fail message
