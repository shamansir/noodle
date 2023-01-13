module Blessed.Internal.Blessed where

import Prelude

import Foreign (Foreign)

data Handler e = Handler

data SProp = SProp String Foreign
data SNode e = SNode (Array SProp) (Array (SNode e)) (Array (Handler e))