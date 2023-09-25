module Noodle.Stateful where


import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (range)
import Data.Array (length) as Array
import Data.Set (Set)
import Data.Foldable (foldr)
import Data.Set as Set


class Stateful f state where
    get :: f -> state
    set :: state -> f -> f


modify :: forall f state. Stateful f state => (state -> state) -> f -> f
modify mf f = set (mf $ get f) f


class StatefulM f m state where -- MonadState ?
    getM :: f -> m state
    setM :: state -> f -> m f


modifyM :: forall f m state. Monad m => StatefulM f m state => (state -> state) -> f -> m f
modifyM mf f =
    getM f <#> mf >>= flip setM f


modifyM' :: forall f m state. Monad m => StatefulM f m state => (state -> m state) -> f -> m f
modifyM' mf f =
    getM f >>= mf >>= flip setM f
    {-
    do
    curState <- getM f
    nextState <- mf curState
    nextF <- setM nextState f
    pure nextF
    -}
    -- getM f >>= (mf >>>)


class StatefulIx f ix state | f -> state, f -> ix where -- use MapWithIndex / FoldableWithIndex ??
    getIx :: ix -> f -> Maybe state
    setIx :: ix -> state -> f -> Maybe f
    indices :: f -> Set ix


count :: forall f state. StatefulIx f Int state => f -> Int
count = indices >>> Set.size


setIx' :: forall f ix state. StatefulIx f ix state => ix -> state -> f -> f
setIx' ix s f =
    fromMaybe f $ setIx ix s f


setAll :: forall f ix state. StatefulIx f ix state => state -> f -> f
setAll s f =
    -- range 0 (count f - 1) # foldr mixf f
    indices f # foldr mixf f
    where
        mixf :: ix -> f -> f
        mixf ix = setIx' ix s


modifyIx :: forall f ix state. StatefulIx f ix state => ix -> (state -> state) -> f -> Maybe f
modifyIx ix mf f =
    getIx ix f >>= \s -> setIx ix (mf s) f


modifyIx' :: forall f ix state. StatefulIx f ix state => ix -> (state -> state) -> f -> f
modifyIx' ix mf f =
    fromMaybe f $ modifyIx ix mf f


modifyAll :: forall f ix state. StatefulIx f ix state => (state -> state) -> f -> f
modifyAll mf f =
    indices f # foldr mixf f
    where
        mixf :: ix -> f -> f
        mixf ix = modifyIx' ix mf