module Noodle.Toolkit2
  ( Toolkit
  , Family
  , family
  , from, toRecord
  , spawn
  , unsafeSpawn
  , toStates, ToState
  )
  where

import Prelude

import Data.Semigroup (class Semigroup)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, reflectSymbol, reifySymbol)
import Data.List as List
import Data.List (List)
import Data.Maybe as Maybe
import Data.Maybe (Maybe(..))
import Data.Foldable (foldr, class Foldable)
import Data.Traversable (sequence)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))

import Type.Proxy (Proxy(..))

import Effect.Console (log)
import Effect.Class (class MonadEffect, liftEffect)

import Prim.Row (class Cons, class Lacks, class Nub)
import Prim.RowList (RowList, class RowToList)
import Record as Record
import Record.Extra (class Keys)
import Record.Extra as Record
import Record.Builder (Builder)
import Record.Builder as Builder


import Heterogeneous.Folding as H
import Heterogeneous.Mapping as H

import Noodle.Node (Node)
import Noodle.Node as Node

newtype Toolkit (nodes :: Row Type) = Toolkit (Record nodes)


data Family (s :: Symbol) = Family String


family :: forall proxy s. IsSymbol s => proxy s -> Family s
family f = Family $ reflectSymbol f


produceFamily :: forall s. IsSymbol s => String -> Family s
produceFamily s = reifySymbol s (\_ -> (Family s :: Family s))


from :: forall (nodes :: Row Type). Record nodes -> Toolkit nodes
from = Toolkit


toRecord :: forall (nodes :: Row Type). Toolkit nodes -> Record nodes
toRecord (Toolkit tk) = tk


data ToState = ToState


class GetState from state where
    getState :: forall proxy s. IsSymbol s => proxy s -> from -> state


instance toState ::
  (IsSymbol sym, GetState a b) =>
  H.MappingWithIndex ToState (Proxy sym) a b where
    mappingWithIndex ToState prop a = getState prop a


{- instance getStateNodeFn ::
  (IsSymbol sym) =>
  GetState (Node.NodeFn state d) state where
    getState sym nodeFn = 0
-}

{-
instance toStateNodeFn ::
  (IsSymbol sym) =>
  H.MappingWithIndex ToState (SProxy sym) (Node.NodeFn state d) state where
    mappingWithIndex ToState prop a = getState prop a
-}


toStates ∷ ∀ (nodes :: Row Type) (states :: Row Type). H.HMapWithIndex ToState (Record nodes) (Record states) ⇒ Toolkit nodes → Record states
toStates = H.hmapWithIndex ToState <<< toRecord



spawn
    :: forall m s (nodes :: Row Type) (r' ∷ Row Type) state d
     . IsSymbol s
    => Cons s (Node.NodeFn state d) r' nodes
    => MonadEffect m
    => Toolkit nodes
    -> Family s
    -> d
    -> m (Node state d)
spawn (Toolkit tk) sym st =
    Record.get sym tk
        # Node.make' st


unsafeSpawn
    :: forall m s (nodes :: Row Type) (r' ∷ Row Type) state d ks
     . Keys ks
    => IsSymbol s
    => RowToList nodes ks
    => Cons s (Node.NodeFn state d) r' nodes
    => MonadEffect m
    => Toolkit nodes
    -> String
    -> d
    -> m (Maybe (Family s /\ Node state d))
unsafeSpawn (Toolkit tk) s d =
    if List.elem s $ Record.keys tk then
        let family_ = produceFamily s :: Family s
        in Just <$> ((/\) family_) <$> (spawn (Toolkit tk) family_ d)
    else pure Nothing