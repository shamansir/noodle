module Xodus.Toolkit.Requests where

import Effect (Effect)
import Effect.Aff (Milliseconds(..), Aff, launchAff_, delay)

import Prelude

someFunction :: Effect Unit
someFunction = launchAff_ $ delay $ Milliseconds 2000.0
