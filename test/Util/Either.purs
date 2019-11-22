module Rpd.Test.Util.Either
    ( getOrFail
    ) where


import Prelude (show, const, pure, class Show, (>>=), ($))
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
