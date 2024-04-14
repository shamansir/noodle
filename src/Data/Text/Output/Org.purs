module Data.Text.Output.Org where

import Prelude

import Type.Proxy (Proxy(..))
import Data.Text.Output (OutputKind, class Renderer, layout, Support)


foreign import data Org :: OutputKind


markdown = Proxy :: _ Org