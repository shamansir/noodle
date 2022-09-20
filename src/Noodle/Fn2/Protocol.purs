module Noodle.Fn.Protocol
  ( Protocol
  , mkDefault
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



class Channel a d | d -> a where
    adapt :: d -> Maybe a
    lift :: a -> d


type Protocol d = -- a.k.a. Transport
    { receive :: (forall a i. Channel a d => i -> Effect (Maybe a)) -- FIXME: Get rid of `Effect`
    , receive' :: (forall a i. Channel a d => i -> Effect (Maybe d)) -- FIXME: Get rid of `Effect`
    , send :: (forall a o. Channel a d => o -> a -> Effect Unit)
    , sendIn :: (forall a i. Channel a d => i -> a -> Effect Unit)
    , last :: (forall i.  Unit -> Effect (Maybe i))-- FIXME: Get rid of `Effect`
    }


-- type Tracker k v = Ref (k /-> v)


-- newTracker :: forall k v. Effect (Tracker k v)
-- newTracker = Ref.new Map.empty


-- put :: forall k v. Ord k => k -> v -> Tracker k v -> Effect Unit
-- put k v = Ref.modify_ (Map.insert k v)


-- check :: forall k v. Ord k => k -> Tracker k v -> Effect (Maybe v)
-- check k tracker =
--     Ref.read tracker <#> Map.lookup k


mkDefault
    :: forall inputs outputs d
     . Record inputs
    -> Record outputs
    -> Effect (protocol :: Protocol d /\ Record outputs)
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