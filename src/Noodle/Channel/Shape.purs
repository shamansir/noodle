module Noodle.Channel.Shape
    where


import Prelude ((>>>), (<<<), (>>=), (<$>), ($), (<*>), (=<<), flip)


import Data.Maybe (Maybe(..))
-- import Noodle.Shape (Shape)

import Data.Functor (class Functor)
import Data.Functor.Invariant (class Invariant)
import Data.Profunctor (class Profunctor)
import Data.Profunctor as Profunctor


type Id = String


data Shape d a =
    Shape
        { id :: String
        , default :: a -- d?
        , accept :: d -> Maybe a -- TODO: remove, move to typeclass, can be used on `Node.connect` / `Node.send`
        --, adapt :: d -> d
        , isHot :: Boolean
        , hidden :: Boolean
        }


type Shape' d = Shape d d


class IsShape m where
    accept :: forall a d. m d a -> d -> Maybe a
    --default :: a


instance functorShape :: Functor (Shape a) where
    map :: forall d a b. (a -> b) -> Shape d a -> Shape d b
    map f (Shape { id, default, accept, isHot, hidden }) =
        Shape
            { id, default : f default, isHot, hidden
            , accept : ((<$>) f) <$> accept
            }


instance profunctorShape :: Profunctor Shape where
    dimap :: forall a b c d. (b -> a) -> (c -> d) -> Shape a c -> Shape b d
    dimap f g s =
        lcmap f (g <$> s)


instance shapeIsShape :: IsShape (Shape) where
    accept :: forall a d. Shape d a -> d -> Maybe a
    accept (Shape s) = s.accept


id :: forall a d. Shape d a -> String
id (Shape { id }) = id


default :: forall a d. a -> Shape d a -> Shape d a
default default' (Shape def) =
    Shape def { default = default' }


getDefault :: forall a d. Shape d a -> a
getDefault (Shape { default }) = default


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


isHidden :: forall a d. Shape a d -> Boolean
isHidden (Shape def) = def.hidden


hidden :: forall a d. Shape a d -> Shape a d
hidden (Shape def) =
    Shape def { hidden = true }


visible :: forall a d. Shape a d -> Shape a d
visible (Shape def) =
    Shape def { hidden = false }


shape :: forall a d. Id -> a -> (d -> Maybe a) -> Shape d a
shape id v accept =
    Shape
        { id, default : v, accept
        , isHot : true
        , hidden : false
        }


shapeBy :: forall d. Id -> d -> (d -> Boolean) -> Shape d d
shapeBy id v test = shape id v (\d -> if test d then Just d else Nothing)


through :: forall a. Id -> a -> Shape a a
through id = flip (shape id) Just


acceptWith :: forall a d. (a -> Maybe d) -> Shape a d -> Shape a d
acceptWith f (Shape def) = Shape def { accept = f }


lcmap :: forall a b c. (b -> a) -> Shape a c -> Shape b c
lcmap f (Shape { id, default, accept, isHot, hidden }) =
    Shape
        { id
        , default
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
