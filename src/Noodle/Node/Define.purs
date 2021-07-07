module Noodle.Node.Define
    ( Def(..), Receive(..), Pass(..)
    , empty
    , get, set
    , define, defineEffectful
    , receive, pass, pass', passNothing, doNothing
    , lastUpdateAt
    , withFn1, withFn2, withFn3, withFn4, withFn5
    , fromFn1, fromFn2, fromFn3, fromFn4, fromFn5
    , fromFn1', fromFn2', fromFn3', fromFn4', fromFn5'
    )
    where


import Prelude

import Effect (Effect)
import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Data.Map.Extra (type (/->))
import Data.Traversable (sequence)
--import Data.Functor (class Functor)
import Data.Functor.Invariant (class Invariant)


newtype Receive d = Receive { last :: String, fromInlets :: String /-> d }


instance functorReceive :: Functor Receive where
    map f (Receive { last, fromInlets }) = Receive { last, fromInlets : f <$> fromInlets }


newtype Pass d = Pass { toOutlets :: String /-> d }


instance functorPass :: Functor Pass where
    map f (Pass { toOutlets }) = Pass { toOutlets : f <$> toOutlets }



data Def a d = Def a (Receive d -> Effect (Pass d))


instance invariantDef :: Invariant (Def a) where
    imap aToB bToA (Def v fn) =
        Def v $ \receive -> (<$>) aToB <$> (fn $ bToA <$> receive)


get :: forall d a. Def a d -> a
get (Def v _) = v


set :: forall d a. a -> Def a d -> Def a d
set v (Def _ fn) = Def v fn


empty :: forall d a. a -> Def a d
empty v = define v $ const $ pass []


define
    :: forall d a
     . a
    -> (Receive d -> Pass d)
    -> Def a d
define v fn = defineEffectful v (pure <<< fn)


defineEffectful
    :: forall d a
     . a
    -> (Receive d -> Effect (Pass d))
    -> Def a d
defineEffectful = Def


{-

defineFolding -- makePastDep
    :: forall d a state
     . a -- use `a` as the state?
    -> state
    -> d
    -> (Receive d -> state -> Effect (state /\ Pass d))
    -> Effect (Node d a)
makeFolding


defineEffectfulFolding
    :: forall d a state
     . a -- use `a` as the state?
    -> state
    -> d
    -> (Receive d -> state -> Effect (state /\ Pass d))
    -> Effect (Node d a)
defineEffectfulFolding

-}


receive :: forall d. String -> Receive d -> Maybe d
receive label (Receive { fromInlets }) = Map.lookup label fromInlets -- unwrap >>> flip Map.lookup


lastUpdateAt :: forall d. Receive d -> String
lastUpdateAt (Receive { last }) = last


pass :: forall d. Array (String /\ d) -> Pass d
--pass = Pass <<< Map.fromFoldable
pass values = Pass { toOutlets : Map.fromFoldable values }


pass' :: forall d. Array (String /\ Maybe d) -> Pass d
--pass' = Pass <<< Map.fromFoldable <<< Array.mapMaybe sequence
pass' values = Pass { toOutlets : Map.fromFoldable $ Array.mapMaybe sequence $ values }


passNothing :: forall d. Pass d
passNothing = Pass { toOutlets : Map.empty }


doNothing :: forall d. Receive d -> Pass d
doNothing = const $ passNothing


fromFn1 :: forall d a. a -> (d -> d) -> Def a d
fromFn1 v fn =
    define v $ \r -> pass' [ "0" /\ (fn <$> receive "0" r) ]


fromFn1' :: forall d a. a -> (d -> Maybe d) -> Def a d
fromFn1' v fn =
    define v $ \r -> pass' [ "0" /\ (fn =<< receive "0" r) ]


fromFn2 :: forall d a. a -> (d -> d -> d) -> Def a d
fromFn2 v fn =
    define v $ \r -> pass' [ "0" /\ (fn <$> receive "0" r <*> receive "1" r) ]


fromFn2' :: forall d a. a -> (d -> d -> Maybe d) -> Def a d
fromFn2' v fn =
    define v $ \r -> pass' [ "0" /\ (identity =<< fn <$> receive "0" r <*> receive "1" r) ]


fromFn3 :: forall d a. a -> (d -> d -> d -> d) -> Def a d
fromFn3 v fn =
    define v $
        \r -> pass' [ "0" /\ (fn <$> receive "0" r <*> receive "1" r <*> receive "2" r) ]


fromFn3' :: forall d a. a -> (d -> d -> d -> Maybe d) -> Def a d
fromFn3' v fn =
    define v $
        \r -> pass' [ "0" /\ (identity =<< fn <$> receive "0" r <*> receive "1" r <*> receive "2" r) ]


fromFn4 :: forall d a. a -> (d -> d -> d -> d -> d) -> Def a d
fromFn4 v fn =
    define v $
        \r -> pass' [ "0" /\ (fn <$> receive "0" r <*> receive "1" r <*> receive "2" r <*> receive "3" r) ]


fromFn4' :: forall d a. a -> (d -> d -> d -> d -> Maybe d) -> Def a d
fromFn4' v fn =
    define v $
        \r -> pass' [ "0" /\ (identity =<< fn <$> receive "0" r <*> receive "1" r <*> receive "2" r <*> receive "3" r) ]


fromFn5 :: forall d a. a -> (d -> d -> d -> d -> d -> d) -> Def a d
fromFn5 v fn =
    define v $ \r -> pass'
        [ "0" /\ (fn <$> receive "0" r <*> receive "1" r <*> receive "2" r <*> receive "3" r <*> receive "4" r) ]


fromFn5' :: forall d a. a -> (d -> d -> d -> d -> d -> Maybe d) -> Def a d
fromFn5' v fn =
    define v $ \r -> pass'
        [ "0" /\ (identity =<< fn <$> receive "0" r <*> receive "1" r <*> receive "2" r <*> receive "3" r <*> receive "4" r) ]


withFn1 :: forall d. (d -> d) -> String -> Receive d -> Maybe d
withFn1 fn inlet r = fn <$> receive inlet r


withFn2 :: forall d. (d -> d -> d) -> String -> String -> Receive d -> Maybe d
withFn2 fn inletA inletB r = fn <$> receive inletA r <*> receive inletB r


withFn3 :: forall d. (d -> d -> d -> d) -> String -> String -> String -> Receive d -> Maybe d
withFn3 fn inletA inletB inletC r = fn <$> receive inletA r <*> receive inletB r <*> receive inletC r


withFn4 :: forall d. (d -> d -> d -> d -> d) -> String -> String -> String -> String -> Receive d -> Maybe d
withFn4 fn inletA inletB inletC inletD r = fn <$> receive inletA r <*> receive inletB r <*> receive inletC r <*> receive inletD r


withFn5 :: forall d. (d -> d -> d -> d -> d -> d) -> String -> String -> String -> String -> String -> Receive d -> Maybe d
withFn5 fn inletA inletB inletC inletD inletE r =
    fn <$> receive inletA r <*> receive inletB r <*> receive inletC r <*> receive inletD r <*> receive inletE r
