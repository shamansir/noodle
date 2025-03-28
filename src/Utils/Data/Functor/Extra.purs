module Data.Functor.Extra where

import Prelude


infixl 5 mapmap as <$$>
infixl 5 mapmapmap as <$$$>


infixl 1 mapmapFlipped as <##>
infixl 1 mapmapmapFlipped as <###>


mapmap :: forall f g a b. Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
mapmap = map <<< map


mapmapmap :: forall f g h a b. Functor f => Functor g => Functor h => (a -> b) -> f (g (h a)) -> f (g (h b))
mapmapmap = mapmap <<< map


mapmapFlipped :: forall f g a b. Functor f => Functor g => f (g a) -> (a -> b) -> f (g b)
mapmapFlipped = flip mapmap


mapmapmapFlipped :: forall f g h a b. Functor f => Functor g => Functor h => f (g (h a)) -> (a -> b) -> f (g (h b))
mapmapmapFlipped = flip mapmapmap