module Data.Text.Output.Markdown where

import Prelude

import Type.Proxy (Proxy(..))
import Data.Text.Output (OutputKind, class Renderer, layout, Support)


foreign import data Markdown :: OutputKind


markdown = Proxy :: _ Markdown