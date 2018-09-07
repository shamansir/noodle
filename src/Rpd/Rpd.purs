module Rpd
    ( init, run, run'
    ) where

import Prelude


import Data.Either (either)
import Control.Monad.Except.Trans (runExceptT)

import Effect (Effect)

import Rpd.API as API
import Rpd.Network (Network)


run
    :: forall a r
     . (API.RpdError -> r)
    -> (a -> r)
    -> API.Rpd a
    -> Effect r
run onError onSuccess =
    run' (pure <<< onError) (pure <<< onSuccess)


run'
    :: forall a r
     . (API.RpdError -> Effect r)
    -> (a -> Effect r)
    -> API.Rpd a
    -> Effect r
run' onError onSuccess rpd =
    runExceptT rpd >>= either onError onSuccess
    -- FIXME: we should also call all the cancelers left in the network, before "exiting"


init :: forall d. String -> API.Rpd (Network d)
init = API.init
