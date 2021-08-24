module Noodle.Node.Define
    ( Def(..), Receive(..), Pass(..)
    , empty
    , define, defineEffectful
    , fromFn, fromFnEffectful
    , receive, pass, pass', passNothing, passThrough, passThrough', doNothing, always, alwaysOne
    , lastUpdateAt
    , dimensions, processWith, processEffectfulWith
    --, withFn1, withFn2, withFn3, withFn4, withFn5
    -- , fromFn1, fromFn2, fromFn3, fromFn4, fromFn5
    --, fromFn1', fromFn2', fromFn3', fromFn4', fromFn5'
    )
    where


import Prelude

import Effect (Effect)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Tuple (curry, uncurry)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.Map.Extra (type (/->))
import Data.Traversable (sequence)
import Data.Foldable (foldr)
--import Data.Functor (class Functor)
import Data.Functor.Invariant (class Invariant, imap)
import Data.Bifunctor (lmap, rmap)
import Data.Newtype (unwrap)


import Noodle.Node.Shape (Shape(..), Inlets, Outlets, InletId, OutletId)
import Noodle.Node.Shape as Shape
import Noodle.Channel.Shape as Channel


newtype Receive d = Receive { last :: InletId, fromInlets :: InletId /-> d }


instance functorReceive :: Functor Receive where
    map f (Receive { last, fromInlets }) = Receive { last, fromInlets : f <$> fromInlets }


newtype Pass d = Pass { toOutlets :: OutletId /-> d }


instance functorPass :: Functor Pass where
    map f (Pass { toOutlets }) = Pass { toOutlets : f <$> toOutlets }


data Def d = Def (Shape d) (Receive d -> Effect (Pass d)) -- TODO: Define.Shapeless, still


instance invariantDef :: Invariant Def where
    imap aToB bToA (Def shape fn) =
        Def (imap aToB bToA shape) $ \receive -> (<$>) aToB <$> (fn $ bToA <$> receive)


empty :: forall d. Def d
empty = define Shape.noInlets Shape.noOutlets $ const $ pass []


define
    :: forall d
     . Inlets d
    -> Outlets d
    -> (Receive d -> Pass d)
    -> Def d
define inlets outlets fn = defineEffectful inlets outlets (pure <<< fn)


defineEffectful
    :: forall d
     . Inlets d
    -> Outlets d
    -> (Receive d -> Effect (Pass d))
    -> Def d
defineEffectful i o = Def $ Shape.make i o


fromFn
    :: forall d
     . (Receive d -> Pass d)
    -> Def d
fromFn = define Shape.noInlets Shape.noOutlets


fromFnEffectful
    :: forall d
     . (Receive d -> Effect (Pass d))
    -> Def d
fromFnEffectful = defineEffectful Shape.noInlets Shape.noOutlets


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


receive :: forall d. InletId -> Receive d -> Maybe d
receive label (Receive { fromInlets }) = Map.lookup label fromInlets -- unwrap >>> flip Map.lookup


lastUpdateAt :: forall d. Receive d -> InletId
lastUpdateAt (Receive { last }) = last


pass :: forall d. Array (OutletId /\ d) -> Pass d
--pass = Pass <<< Map.fromFoldable
pass values = Pass { toOutlets : Map.fromFoldable values }


always :: forall d. Array (OutletId /\ d) -> Receive d -> Pass d
always = const <<< pass


alwaysOne :: forall d. OutletId /\ d -> Receive d -> Pass d
alwaysOne = always <<< Array.singleton


pass' :: forall d. Array (OutletId /\ Maybe d) -> Pass d
--pass' = Pass <<< Map.fromFoldable <<< Array.mapMaybe sequence
pass' values = Pass { toOutlets : Map.fromFoldable $ Array.mapMaybe sequence $ values }


passNothing :: forall d. Pass d
passNothing = Pass { toOutlets : Map.empty }


passThrough :: forall d. Receive d -> Pass d
passThrough (Receive { fromInlets }) = Pass { toOutlets : fromInlets }


passThrough' :: forall d. Array InletId -> Receive d -> Pass d
passThrough' inlets (Receive { fromInlets }) =
    Pass
        { toOutlets :
            Map.fromFoldable
                 $  Array.catMaybes
                 $  (\inlet ->
                        ((/\) inlet) <$> Map.lookup inlet fromInlets
                    )
                <$> inlets
         }


doNothing :: forall d. Receive d -> Pass d
doNothing = const $ passNothing


getShape :: forall d. Def d -> Inlets d /\ Outlets d
getShape (Def shape _) = unwrap shape


dimensions :: forall d. Def d -> Int /\ Int
dimensions (Def shape _) = Shape.dimensions shape


processWith :: forall d. (Receive d -> Pass d) -> Def d -> Def d
processWith fn = processEffectfulWith (pure <<< fn)


processEffectfulWith :: forall d. (Receive d -> Effect (Pass d)) -> Def d -> Def d
processEffectfulWith fn (Def shape _) = Def shape fn


{-
fromFn1 :: forall d a. a -> (d -> d) -> Def d
fromFn1 v fn =
    define v $ \r -> pass' [ "0" /\ (fn <$> receive "0" r) ]


fromFn1' :: forall d a. a -> (d -> Maybe d) -> Def d
fromFn1' v fn =
    define v $ \r -> pass' [ "0" /\ (fn =<< receive "0" r) ]


fromFn2 :: forall d a. a -> (d -> d -> d) -> Def d
fromFn2 v fn =
    define v $ \r -> pass' [ "0" /\ (fn <$> receive "0" r <*> receive "1" r) ]


fromFn2' :: forall d a. a -> (d -> d -> Maybe d) -> Def d
fromFn2' v fn =
    define v $ \r -> pass' [ "0" /\ (identity =<< fn <$> receive "0" r <*> receive "1" r) ]


fromFn3 :: forall d a. a -> (d -> d -> d -> d) -> Def d
fromFn3 v fn =
    define v $
        \r -> pass' [ "0" /\ (fn <$> receive "0" r <*> receive "1" r <*> receive "2" r) ]


fromFn3' :: forall d a. a -> (d -> d -> d -> Maybe d) -> Def d
fromFn3' v fn =
    define v $
        \r -> pass' [ "0" /\ (identity =<< fn <$> receive "0" r <*> receive "1" r <*> receive "2" r) ]


fromFn4 :: forall d a. a -> (d -> d -> d -> d -> d) -> Def d
fromFn4 v fn =
    define v $
        \r -> pass' [ "0" /\ (fn <$> receive "0" r <*> receive "1" r <*> receive "2" r <*> receive "3" r) ]


fromFn4' :: forall d a. a -> (d -> d -> d -> d -> Maybe d) -> Def d
fromFn4' v fn =
    define v $
        \r -> pass' [ "0" /\ (identity =<< fn <$> receive "0" r <*> receive "1" r <*> receive "2" r <*> receive "3" r) ]


fromFn5 :: forall d a. a -> (d -> d -> d -> d -> d -> d) -> Def d
fromFn5 v fn =
    define v $ \r -> pass'
        [ "0" /\ (fn <$> receive "0" r <*> receive "1" r <*> receive "2" r <*> receive "3" r <*> receive "4" r) ]


fromFn5' :: forall d a. a -> (d -> d -> d -> d -> d -> Maybe d) -> Def d
fromFn5' v fn =
    define v $ \r -> pass'
        [ "0" /\ (identity =<< fn <$> receive "0" r <*> receive "1" r <*> receive "2" r <*> receive "3" r <*> receive "4" r) ] -}


{-
withFn1 :: forall d. (d -> d) -> InletId -> Receive d -> Maybe d
withFn1 fn inlet r = fn <$> receive inlet r


withFn2 :: forall d. (d -> d -> d) -> InletId -> InletId -> Receive d -> Maybe d
withFn2 fn inletA inletB r = fn <$> receive inletA r <*> receive inletB r


withFn3 :: forall d. (d -> d -> d -> d) -> InletId -> InletId -> InletId -> Receive d -> Maybe d
withFn3 fn inletA inletB inletC r = fn <$> receive inletA r <*> receive inletB r <*> receive inletC r


withFn4 :: forall d. (d -> d -> d -> d -> d) -> InletId -> InletId -> InletId -> InletId -> Receive d -> Maybe d
withFn4 fn inletA inletB inletC inletD r = fn <$> receive inletA r <*> receive inletB r <*> receive inletC r <*> receive inletD r


withFn5 :: forall d. (d -> d -> d -> d -> d -> d) -> InletId -> InletId -> InletId -> InletId -> InletId -> Receive d -> Maybe d
withFn5 fn inletA inletB inletC inletD inletE r =
    fn <$> receive inletA r <*> receive inletB r <*> receive inletC r <*> receive inletD r <*> receive inletE r
-}


{-
addInlet :: forall d. InletId -> Channel.Shape d -> Def d -> Def d
-- addInlet name shape = (<$>) (lmap $ Map.insert name shape)
addInlet name shape (Def shapes fn) = Def ((lmap $ Map.insert name shape) shapes) fn


addOutlet :: forall d. OutletId -> (forall a. Channel.Shape a d) -> Def d -> Def d
-- addOutlet name shape = (<$>) (rmap $ Map.insert name shape)
addOutlet name shape (Def shapes fn) = Def ((rmap $ Map.insert name shape) shapes) fn -}


reshape :: forall d. Shape.Inlets d -> Shape.Outlets d -> Def d -> Def d
reshape inlets outlets (Def _ fn) =
    Def (Shape.make inlets outlets) fn
        -- FIXME: update the handler to monitor hot/cold inlets as well


{- reshapeInlet :: forall d. InletId -> Channel.Shape d -> Def d -> Def d
reshapeInlet = addInlet -}


{- reshapeOutlet :: forall d. String -> Channel.Shape d -> Def d -> Def d
reshapeOutlet = addOutlet -}


inletShape :: forall d. InletId -> Def d -> Maybe (Channel.Shape d d)
inletShape inlet (Def shape _) = Shape.inlet inlet shape


outletShape :: forall d. OutletId -> Def d -> Maybe (Channel.Shape d d)
outletShape outlet (Def shape _) = Shape.outlet outlet shape
