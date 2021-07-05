module Noodle.Channel.Shape
    where


import Prelude ((>>>), (<<<), (>>=), (<$>))


import Data.Maybe (Maybe(..))
-- import Noodle.Shape (Shape)

import Data.Functor.Invariant (class Invariant)


instance Invariant Shape where
    imap :: forall a b. (a -> b) -> (b -> a) -> Shape a -> Shape b
    imap toB toA (Shape { default, accept, isHot }) =
        Shape { default : toB default, accept : \b -> toB <$> accept (toA b), isHot }


data Shape d =
    Shape
        { default :: d
        , accept :: d -> Maybe d
        --, adapt :: d -> d
        , isHot :: Boolean
        }


transform :: forall d. Shape d -> d -> Maybe d
transform (Shape { accept }) = accept


-- TODO: Functor Shape ...


-- TODO: monoid :: Monoid m => Shape m


-- TODO: int :: Shape Int
-- TODO:int = {}


isHot :: forall d. Shape d -> Boolean
isHot (Shape def) = def.isHot


cold :: forall d. Shape d -> Shape d
cold (Shape def) =
    Shape def { isHot = false }


hot :: forall d. Shape d -> Shape d
hot (Shape def) =
    Shape def { isHot = true }


shape :: forall d. d -> Shape d
shape v =
    Shape
        { default : v
        , accept : Just
        , isHot : true
        }


acceptWith :: forall d. (d -> Maybe d) -> Shape d -> Shape d
acceptWith f (Shape def) = Shape def { accept = f }


int :: Int -> Shape Int
int = shape


number :: Number -> Shape Number
number = shape


boolean :: Boolean -> Shape Boolean
boolean = shape

-- TODO