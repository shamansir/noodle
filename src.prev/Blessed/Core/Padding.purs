module Blessed.Core.Padding where

import Prelude


type Padding
    = { top :: Int, left :: Int, right :: Int, bottom :: Int }



all :: Int -> Padding
all n = { top : n, left : n, right : n, bottom : n }