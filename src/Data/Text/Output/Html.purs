module Data.Text.Output.Html where

import Prelude

import Type.Proxy (Proxy(..))
import Data.Text.Output (OutputKind, class Renderer, layout, Support)


foreign import data Html :: OutputKind


html = Proxy :: _ Html