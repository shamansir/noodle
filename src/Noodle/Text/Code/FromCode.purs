module Noodle.Text.FromCode where

import Prelude

import Type.Proxy (Proxy)
import Data.Maybe (Maybe)

import Noodle.Text.Code.Target (Target)

class FromCode (target :: Target) opts a where
    fromCode :: Proxy target -> opts -> Maybe a