module Noodle.Toolkit3
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
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol, reifySymbol)
import Data.List as List
import Data.List (List)
import Data.Maybe as Maybe
import Data.Maybe (Maybe(..))
import Data.Foldable (foldr, class Foldable)
import Data.Traversable (sequence)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.Function.Uncurried


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

import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Fn2 (Fn)
import Noodle.Fn2 as Fn
import Noodle.Fn2.Process (ProcessM)
import Noodle.Fn2.Process as P

newtype Toolkit (nodes :: Row Type) = Toolkit (Record nodes)


data Family (s :: Symbol) = Family String


{- type NodeDesc state is os m =
    state /\ Record is /\ Record os /\ Node state is os m -}
    {-
    { defaultState :: state
    , defaultInputs :: Record is
    , defaultOutputs :: Record os
    , node :: Node state is os m
    } -}


type FnDesc state is os m =
    state /\ Record is /\ Record os /\ Fn state is os m


family :: forall proxy f. IsSymbol f => proxy f -> Family f
family f = Family $ reflectSymbol f


produceFamily :: forall f. IsSymbol f => String -> Family f
produceFamily f = reifySymbol f (\_ -> (Family f :: Family f))


from :: forall (nodes :: Row Type). Record nodes -> Toolkit nodes
from = Toolkit


toRecord :: forall (nodes :: Row Type). Toolkit nodes -> Record nodes
toRecord (Toolkit tk) = tk


data ToState = ToState


class GetState from state where
    getState :: forall proxy s. IsSymbol s => proxy s -> from -> state


instance toState ::
  (IsSymbol sym, GetState a b) =>
  H.MappingWithIndex ToState (SProxy sym) a b where
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
    :: forall f (nodes :: Row Type) (r' ∷ Row Type) state is os m
     . IsSymbol f
    => Cons f (FnDesc state is os m) r' nodes
    => MonadEffect m
    => Toolkit nodes
    -> Family f
    -> Int
    -> m (Node state is os m)
spawn (Toolkit tk) fsym num =
    Record.get fsym tk
        # makeNode
    where
      makeNode (state /\ is /\ os /\ fn) = Node.make' ("foo" /\ num) state is os fn


unsafeSpawn
    :: forall f (nodes :: Row Type) (r' ∷ Row Type) state is os m ks
     . Keys ks
    => IsSymbol f
    => RowToList nodes ks
    => Cons f (FnDesc state is os m) r' nodes
    => MonadEffect m
    => Toolkit nodes
    -> String
    -> Int
    -> m (Maybe (Family f /\ Node state is os m))
unsafeSpawn (Toolkit tk) s n =
    if List.elem s $ Record.keys tk then
        let family_ = produceFamily s :: Family f
        in Just <$> ((/\) family_) <$> (spawn (Toolkit tk) family_ n)
    else pure Nothing