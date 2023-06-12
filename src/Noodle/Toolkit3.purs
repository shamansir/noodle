module Noodle.Toolkit3
  ( Toolkit
  , from
  , name
  , nodeFamilies
  , familiesOrder
  , spawn
  , toRecord
  --, toStates
  , unsafeSpawn
  , unsafeSpawnR
  , familyDefs
  , familyDefsIndexed
  , mapFamilies, mapFamiliesIndexed
  --, inputsFromDef, outputsFromDef
  , toShapes, toRepr
  )
  where

import Prelude

import Data.Symbol (reifySymbol)
import Data.List as List
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.SProxy (proxify, reflect')


import Effect.Class (class MonadEffect)

-- import Prim.Row (class Cons, class Lacks, class Nub)
-- import Prim.RowList (RowList, class RowToList)
import Record (get) as Record
import Record.Unsafe as RecordU
-- import Record.Extra (class Keys)
import Record.Extra (keys) as Record
import Type.Proxy (Proxy(..))

import Data.SOrder (SOrder, class HasSymbolsOrder)
import Data.SOrder (instantiate) as SOrder


-- import Heterogeneous.Folding as H
-- import Heterogeneous.Mapping as H
import Noodle.Family.Def as Family
import Noodle.Toolkit3.Has as Has
import Noodle.Node2.MapsFolds as NM
import Noodle.Node2.MapsFolds as NF
import Noodle.Node2.MapsFolds.Repr as NR
import Noodle.Toolkit3.MapsFolds as TM
import Noodle.Toolkit3.MapsFolds as TF
import Noodle.Toolkit3.MapsFolds.Repr as TR

import Noodle.Id
import Noodle.Node2 (Node)
import Noodle.Node2 as Node

import Unsafe.Coerce (unsafeCoerce)


type Name = String


data Toolkit :: Type -> Row Type -> Type
data Toolkit gstate (families :: Row Type) = Toolkit Name SOrder (Record families)


{- type NodeDesc state is os m =
    state /\ Record is /\ Record os /\ Node state is os m -}
    {-
    { defaultState :: state
    , defaultInputs :: Record is
    , defaultOutputs :: Record os
    , node :: Node state is os m
    } -}


from :: forall gstate (families :: Row Type) (forder :: SOrder). HasSymbolsOrder forder families => Name -> Proxy forder -> Record families -> Toolkit gstate families
from name forder = Toolkit name $ SOrder.instantiate (Proxy :: _ families) forder


toRecord :: forall gstate (families :: Row Type). Toolkit gstate families -> Record families
toRecord (Toolkit _ _ tk) = tk


familyDefs
    :: forall gstate families x rl
     . TF.Fold rl families Array x
    => Toolkit gstate families
    -> Array x
familyDefs (Toolkit _ _ defs) = TF.hfoldl defs  -- FIXME: ORDER


familiesOrder :: forall gstate families. Toolkit gstate families -> SOrder
familiesOrder (Toolkit _ forder _) = forder


familyDefsIndexed
    :: forall gstate families x rl
     . TF.FoldI rl families Array x
    => Toolkit gstate families
    -> Array x
familyDefsIndexed (Toolkit _ _ defs) = TF.hfoldlWithIndex defs -- FIXME: ORDER


-- toStates ∷ ∀ gstate rl (families :: Row Type) (states :: Row Type). TM.ToStates rl families states ⇒ Toolkit gstate families → Record states
-- toStates = TM.toStates <<< toRecord


mapFamilies
    :: forall gstate families families' rl x
     . TM.Map rl families families' x
    => Toolkit gstate families
    -> Record families'
mapFamilies (Toolkit _ _ defs) =
    TM.hmap (Proxy :: Proxy x) defs



mapFamiliesIndexed
    :: forall gstate families families' rl x
     . TM.ConvertFamilyDefIndexedTo x
    => TM.MapI rl families families' x
    => Toolkit gstate families
    -> Record families'
mapFamiliesIndexed (Toolkit _ _ defs) =
    TM.hmapWithIndex (Proxy :: Proxy x) defs


toShapes
    :: forall gstate fs families shapes
     . TM.MapToShapes fs families shapes
    => Toolkit gstate families
    -> Record shapes
toShapes (Toolkit _ _ defs) =
    TM.toShapes defs


toRepr
    :: forall gstate fs families reprs repr
     . TR.ExtractReprs fs families reprs repr
    => TR.Repr repr
    -> Toolkit gstate families
    -> Record reprs
toRepr repr (Toolkit _ _ defs) =
    TM.toReprs repr defs


spawn
    :: forall f (families :: Row Type) (families' ∷ Row Type) gstate state is os m
     . MonadEffect m
    => Has.HasFamilyDef f families' families (Family.Def state is os m)
    => Toolkit gstate families
    -> Family f
    -> m (Node f state is os m)
spawn (Toolkit _ _ tk) fsym =
    Record.get (proxify fsym) tk
        # makeNode
    where
      makeNode (Family.Def (state /\ is /\ os /\ fn)) = Node.make' (family' fsym) state is os fn


unsafeSpawn
    :: forall f (families :: Row Type) (families' ∷ Row Type) gstate state is os m ks
     . MonadEffect m
    => ListsFamilies families ks
    => Has.HasFamilyDef f families' families (Family.Def state is os m)
    => Toolkit gstate families
    -> Family' f
    -> m (Maybe (Family f /\ Node f state is os m))
unsafeSpawn toolkit@(Toolkit name _ tk) family =
    if List.elem (reflect' family) $ Record.keys tk then
        let (family_ :: Family f) = reifySymbol (reflect' family) unsafeCoerce
        in Just <$> ((/\) family_) <$> (spawn toolkit family_)
    else pure Nothing


unsafeSpawnR
    :: forall f (families :: Row Type) (families' ∷ Row Type) gstate state is os m ks
     . MonadEffect m
    => ListsFamilies families ks
    => Has.HasFamilyDef' f families' families (Family.Def state is os m)
    => Toolkit gstate families
    -> FamilyR
    -> m (Maybe (Node f state is os m))
unsafeSpawnR toolkit@(Toolkit _ name tk) family =
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
name (Toolkit name _ _) = name


nodeFamilies :: forall ks gstate families. ListsFamilies families ks => Toolkit gstate families -> List FamilyR
nodeFamilies (Toolkit _ order _) = keysToFamiliesR order (Proxy :: Proxy families)


{-
inputsFromDef :: forall rli state is os m. HasInputsAt is rli => Family.Def state is os m -> List InputR
inputsFromDef _ = keysToInputsR (Proxy :: Proxy is)


outputsFromDef :: forall rlo state is os m. HasOutputsAt os rlo => Family.Def state is os m -> List OutputR
outputsFromDef _ = keysToOutputsR (Proxy :: Proxy os)
-}