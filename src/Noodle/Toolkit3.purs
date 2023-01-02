module Noodle.Toolkit3
  ( Toolkit
  , from, toRecord
  , spawn
  , unsafeSpawn
  , toStates, ToState
  , name
  , nodeFamilies
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

import Noodle.Node2 (Node, Family)
import Noodle.Node2 as Node
import Noodle.Fn2 (Fn)
import Noodle.Fn2 as Fn
import Noodle.Fn2.Process (ProcessM)
import Noodle.Fn2.Process as P

import Unsafe.Coerce (unsafeCoerce)


type Name = String


data Toolkit :: Type -> Row Type -> Type
data Toolkit state (nodes :: Row Type) = Toolkit Name (Record nodes)


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


from :: forall state (nodes :: Row Type). Name -> Record nodes -> Toolkit state nodes
from = Toolkit


toRecord :: forall state (nodes :: Row Type). Toolkit state nodes -> Record nodes
toRecord (Toolkit _ tk) = tk


data ToState = ToState


class GetState from state where
    -- getState :: forall proxy s. IsSymbol s => proxy s -> from -> state
    getState :: forall f. IsSymbol f => Family f -> from -> state


produceFamily :: forall proxy f. IsSymbol f => proxy f -> Family f
produceFamily proxy = reifySymbol (reflectSymbol proxy) unsafeCoerce


instance toState ::
  (IsSymbol sym, GetState a b) =>
  H.MappingWithIndex ToState (SProxy sym) a b where
    mappingWithIndex ToState prop a = getState (produceFamily prop) a


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


toStates ∷ ∀ ts (nodes :: Row Type) (states :: Row Type). H.HMapWithIndex ToState (Record nodes) (Record states) ⇒ Toolkit ts nodes → Record states
toStates = H.hmapWithIndex ToState <<< toRecord



spawn
    :: forall f (nodes :: Row Type) (r' ∷ Row Type) ts state is os m
     . IsSymbol f
    => Cons f (FnDesc state is os m) r' nodes
    => MonadEffect m
    => Toolkit ts nodes
    -> Family f
    -> m (Node f state is os m)
spawn (Toolkit _ tk) fsym =
    Record.get fsym tk
        # makeNode
    where
      makeNode (state /\ is /\ os /\ fn) = Node.make' fsym state is os fn


unsafeSpawn
    :: forall f (nodes :: Row Type) (r' ∷ Row Type) ts state is os m ks
     . Keys ks
    => IsSymbol f
    => RowToList nodes ks
    => Cons f (FnDesc state is os m) r' nodes
    => MonadEffect m
    => Toolkit ts nodes
    -> String
    -> m (Maybe (Family f /\ Node f state is os m))
unsafeSpawn toolkit@(Toolkit name tk) family =
    if List.elem family $ Record.keys tk then
        let (family_ :: Family f) = reifySymbol family unsafeCoerce
        in Just <$> ((/\) family_) <$> (spawn toolkit family_)
    else pure Nothing


name :: forall state nodes. Toolkit state nodes -> Name
name (Toolkit name _) = name


nodeFamilies :: forall ks state nodes. Keys ks => RowToList nodes ks => Toolkit state nodes -> List String
nodeFamilies (Toolkit _ tk) = Record.keys tk


-- nodeFamilies' :: forall ks state nodes. Keys ks => RowToList nodes ks => Toolkit state nodes -> List (forall f. IsSymbol f => Family f)
-- nodeFamilies' :: forall ks state nodes. Keys ks => RowToList nodes ks => Toolkit state nodes -> List (forall f. Family f)
-- nodeFamilies' (Toolkit _ tk) = (\key -> reifySymbol key unsafeCoerce) <$> Record.keys tk