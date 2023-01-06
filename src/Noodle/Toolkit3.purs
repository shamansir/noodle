module Noodle.Toolkit3
  ( Toolkit
  , from
  , name
  , nodeFamilies
  , spawn
  , toRecord
  , toStates
  , unsafeSpawn
  , unsafeSpawnR
  , familyDefs
  , familyDefsIndexed
  , mapFamilies, mapFamiliesIndexed
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
import Data.Array ((:))
import Data.Array as Array
import Data.Function.Uncurried


import Effect.Console (log)
import Effect.Class (class MonadEffect, liftEffect)

import Prim.Row (class Cons, class Lacks, class Nub)
import Prim.RowList (RowList, class RowToList)
import Record as Record
import Record.Unsafe as RecordU
import Record.Extra (class Keys)
import Record.Extra as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))


import Heterogeneous.Folding as H
import Heterogeneous.Mapping as H

import Noodle.Toolkit3.Has as Has
import Noodle.Toolkit3.MapsFolds as TM
import Noodle.Toolkit3.MapsFolds as TF

import Noodle.Id
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Fn2 (Fn)
import Noodle.Fn2 as Fn
import Noodle.Fn2.Process (ProcessM)
import Noodle.Fn2.Process as P

import Unsafe.Coerce (unsafeCoerce)


type Name = String


data Toolkit :: Type -> Row Type -> Type
data Toolkit gstate (families :: Row Type) = Toolkit Name (Record families)


{- type NodeDesc state is os m =
    state /\ Record is /\ Record os /\ Node state is os m -}
    {-
    { defaultState :: state
    , defaultInputs :: Record is
    , defaultOutputs :: Record os
    , node :: Node state is os m
    } -}


from :: forall gstate (families :: Row Type). Name -> Record families -> Toolkit gstate families
from = Toolkit


toRecord :: forall gstate (families :: Row Type). Toolkit gstate families -> Record families
toRecord (Toolkit _ tk) = tk


familyDefs
    :: forall gstate families x rl
     . TF.Fold rl Array x families
    => Toolkit gstate families
    -> Array x
familyDefs (Toolkit _ defs) = TF.hfoldl defs


familyDefsIndexed
    :: forall gstate families x rl
     . TF.FoldI rl Array x families
    => Toolkit gstate families
    -> Array x
familyDefsIndexed (Toolkit _ defs) = TF.hfoldlWithIndex defs


toStates ∷ ∀ gstate (families :: Row Type) (states :: Row Type). H.HMapWithIndex TM.ToState (Record families) (Record states) ⇒ Toolkit gstate families → Record states
toStates = TM.toStates <<< toRecord


mapFamilies
    :: forall gstate families families' rl x
     . TM.Map rl families x families'
    => Toolkit gstate families
    -> Record families'
mapFamilies (Toolkit _ defs) =
    TM.hmap defs



mapFamiliesIndexed
    :: forall gstate families families' rl x
     . TM.MapI rl families x families'
    => Toolkit gstate families
    -> Record families'
mapFamiliesIndexed (Toolkit _ defs) =
    TM.hmapWithIndex defs


spawn
    :: forall f (families :: Row Type) (r' ∷ Row Type) gstate state is os m
     . MonadEffect m
    => Has.HasFamilyDef f r' families (TM.FamilyDef state is os m)
    => Toolkit gstate families
    -> Family f
    -> m (Node f state is os m)
spawn (Toolkit _ tk) fsym =
    Record.get fsym tk
        # makeNode
    where
      makeNode (state /\ is /\ os /\ fn) = Node.make' (family' fsym) state is os fn


unsafeSpawn
    :: forall f (families :: Row Type) (r' ∷ Row Type) gstate state is os m ks
     . MonadEffect m
    => ListsFamilies families ks
    => Has.HasFamilyDef f r' families (TM.FamilyDef state is os m)
    => Toolkit gstate families
    -> Family' f
    -> m (Maybe (Family f /\ Node f state is os m))
unsafeSpawn toolkit@(Toolkit name tk) family =
    if List.elem (reflect' family) $ Record.keys tk then
        let (family_ :: Family f) = reifySymbol (reflect' family) unsafeCoerce
        in Just <$> ((/\) family_) <$> (spawn toolkit family_)
    else pure Nothing


unsafeSpawnR
    :: forall f (families :: Row Type) (r' ∷ Row Type) gstate state is os m ks
     . MonadEffect m
    => ListsFamilies families ks
    => Has.HasFamilyDef' f r' families (TM.FamilyDef state is os m)
    => Toolkit gstate families
    -> FamilyR
    -> m (Maybe (Node f state is os m))
unsafeSpawnR toolkit@(Toolkit name tk) family =
    if List.elem (reflect' family) $ Record.keys tk then
        RecordU.unsafeGet familyStr tk
            # makeNode
            <#> Just
    else pure Nothing
    where
        familyStr = reflect' family
        (family_ :: Family' f) = reifySymbol familyStr unsafeCoerce
        makeNode (state /\ is /\ os /\ fn) = Node.make' family_ state is os fn


name :: forall gstate families. Toolkit gstate families -> Name
name (Toolkit name _) = name


nodeFamilies :: forall ks gstate families. ListsFamilies families ks => Toolkit gstate families -> List FamilyR
nodeFamilies (Toolkit _ _) = keysToFamiliesR (Proxy :: Proxy families)