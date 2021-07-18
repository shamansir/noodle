module Noodle.Channel.Shape
    where


import Prelude ((>>>), (<<<), (>>=), (<$>), ($), (<*>), (=<<), flip)


import Data.Maybe (Maybe(..))
-- import Noodle.Shape (Shape)

import Data.Functor (class Functor)
import Data.Functor.Invariant (class Invariant)
import Data.Bifunctor (class Bifunctor)


{- instance Invariant Shape where
    imap :: forall a b. (a -> b) -> (b -> a) -> Shape a -> Shape b
    imap toB toA (Shape { default, accept, isHot }) =
        Shape { default : toB default, accept : \b -> toB <$> accept (toA b), isHot } -}


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


instance shapeIsShape :: IsShape (Shape) where
    accept :: forall a d. Shape d a -> d -> Maybe a
    accept (Shape s) = s.accept

{- instance bifunctorShape :: Bifunctor Shape where
    bimap = dimap -}


transform :: forall a d. Shape d a -> d -> Maybe a
transform (Shape { accept }) = accept


-- TODO: Functor Shape ...


-- TODO: monoid :: Monoid m => Shape m


-- TODO: int :: Shape Int
-- TODO:int = {}


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


acceptWith :: forall a d. (a -> Maybe d) -> Shape a d -> Shape a d
acceptWith f (Shape def) = Shape def { accept = f }


{- int :: Int -> Shape Int
int = shape -}


{- number :: forall d. Number -> (Number -> Maybe d) -> Shape Number
number = shape -}
number :: Number -> Shape Number Number
number = flip shape Just


{- boolean :: Boolean -> Shape Boolean
boolean = shape -}

-- TODO


{-
align :: forall a b. (a -> b) -> (b -> a) -> Shape a b -> Shape b b
align f g (Shape { default, accept, isHot, hidden }) =
    Shape
        { default : f default
        , accept : g >>> accept -- <<< ?wh
        , isHot
        , hidden
        } -}
