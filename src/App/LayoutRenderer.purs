module App.LayoutRenderer where

import Prelude

import App.Layout (class IsLayout)

import Data.Typelevel.Undefined (undefined)

{- class IsLayout l <= LayoutRenderer l a trg | l -> a where
    renderItem :: a -> trg -}



render :: forall l a trg. IsLayout l => (a -> trg) -> l a -> trg
render _ _ = undefined