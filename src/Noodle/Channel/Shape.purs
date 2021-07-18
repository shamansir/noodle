module Noodle.Channel.Shape
    where


import Prelude ((>>>), (<<<), (>>=), (<$>), ($), (<*>), (=<<), flip)


import Data.Maybe (Maybe(..))
-- import Noodle.Shape (Shape)

import Data.Functor (class Functor)
import Data.Functor.Invariant (class Invariant)
import Data.Profunctor (class Profunctor)
import Data.Profunctor as Profunctor


data Shape d a =
    Shape
        { default :: a -- d?
        , accept :: d -> Maybe a -- TODO: remove, move to typeclass, can be used on `Node.connect` / `Node.send`
        --, adapt :: d -> d
        , isHot :: Boolean
        , hidden :: Boolean
        }


class IsShape m where
    accept :: forall a d. m d a -> d -> Maybe a
    --default :: a


instance functorShape :: Functor (Shape a) where
    map :: forall d a b. (a -> b) -> Shape d a -> Shape d b
    map f (Shape { default, accept, isHot, hidden }) =
        Shape
            { default : f default, isHot, hidden
            , accept : ((<$>) f) <$> accept
            }


instance profunctorShape :: Profunctor Shape where
    dimap :: forall a b c d. (b -> a) -> (c -> d) -> Shape a c -> Shape b d
    dimap f g s =
        lcmap f (g <$> s)


instance shapeIsShape :: IsShape (Shape) where
    accept :: forall a d. Shape d a -> d -> Maybe a
    accept (Shape s) = s.accept


transform :: forall a d. Shape d a -> d -> Maybe a
transform (Shape { accept }) = accept


isHot :: forall a d. Shape a d -> Boolean
isHot (Shape def) = def.isHot


cold :: forall a d. Shape a d -> Shape a d
cold (Shape def) =
    Shape def { isHot = false }


hot :: forall a d. Shape a d -> Shape a d
hot (Shape def) =
    Shape def { isHot = true }


shape :: forall a d. a -> (d -> Maybe a) -> Shape d a
shape v accept =
    Shape
        { default : v
        , accept : accept
        , isHot : true
        , hidden : false
        }


shape' :: forall a d. (d -> Maybe a) -> a -> Shape d a
shape' = flip shape


shape'' :: forall a d. (a -> d) -> (d -> Maybe a) -> a -> Shape d d
shape'' f toN def = f <$> shape def toN


acceptWith :: forall a d. (a -> Maybe d) -> Shape a d -> Shape a d
acceptWith f (Shape def) = Shape def { accept = f }


lcmap :: forall a b c. (b -> a) -> Shape a c -> Shape b c
lcmap f (Shape { default, accept, isHot, hidden }) =
    Shape
        { default
        , accept : accept <<< f
        , isHot
        , hidden
        }


rmap :: forall a b c. (b -> c) -> Shape a b -> Shape a c
rmap = (<$>)


move :: forall a b. (a -> b) -> (b -> a) -> Shape a a -> Shape b b
move f g shape_ = lcmap g (f <$> shape_)


move' :: forall a b c d. (b -> a) -> (c -> d) -> Shape a c -> Shape b d
move' = Profunctor.dimap


through :: forall a. a -> Shape a a
through = flip shape Just


number :: Number -> Shape Number Number
number = through


number' :: forall a. (Number -> a) -> (a -> Maybe Number) -> Number -> Shape a a
number' = shape''
