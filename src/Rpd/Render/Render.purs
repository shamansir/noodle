module Rpd.Render
    ( Renderer(..)
    , render
    ) where

import Prelude
import Effect (Effect)
import Rpd (Network, Rpd, RpdError, run')

data Renderer d r = Renderer (RpdError -> Effect r) (Network d -> Effect r)


render :: forall d r. Renderer d r -> Rpd (Network d) -> Effect r
render (Renderer onError onSuccess) =
    run' onError onSuccess
