module Noodle.Fn.Protocol
  ( Protocol
  , Tracker
  , check
  , mkDefault
  , newTracker
  )
  where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\))



type Protocol i o d = -- a.k.a. Transport
    { receive :: i -> Effect (Maybe d) -- FIXME: remove effect here, was only needed for tests
    , send :: o -> d -> Effect Unit
    , sendIn :: i -> d -> Effect Unit
    , last :: Unit -> Effect (Maybe i) -- FIXME: remove effect here, was only needed for tests
    }


type Tracker k v = Ref (k /-> v)


newTracker :: forall k v. Effect (Tracker k v)
newTracker = Ref.new Map.empty


put :: forall k v. Ord k => k -> v -> Tracker k v -> Effect Unit
put k v = Ref.modify_ (Map.insert k v)


check :: forall k v. Ord k => k -> Tracker k v -> Effect (Maybe v)
check k tracker =
    Ref.read tracker <#> Map.lookup k


mkDefault
    :: forall i o d
     . Ord i => Ord o
    => Array (i /\ d)
    -> Effect ({ protocol :: Protocol i o d, inputs :: Tracker i d, outputs :: Tracker o d })
mkDefault initials = do
    let initialsMap = Map.fromFoldable initials
    inputs <- newTracker
    outputs <- newTracker
    lastRef <- Ref.new Nothing
    let
        protocol =
            { last : const $ Ref.read lastRef
            , receive : \input -> do
                maybeValAtInput <- check input inputs
                pure $ case maybeValAtInput of
                    Just val -> Just val
                    Nothing -> Map.lookup input initialsMap
            , send : \output val -> put output val outputs
            , sendIn : \input val -> do
                Ref.write (Just input) lastRef
                inputs # put input val
            }
    pure { protocol, inputs, outputs }