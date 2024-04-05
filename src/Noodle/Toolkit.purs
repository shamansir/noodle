module Noodle.Toolkit
  ( Toolkit
  , from
  , name
  , nodeFamilies
  , familiesOrder
  , spawn
  , toRecord
  --, toStates
  , unsafeSpawn
  , unsafeSpawnR, unsafeSpawnR'
  , familyDefs
  , familyDefsIndexed
  , mapFamilies, mapFamiliesIndexed
  --, inputsFromDef, outputsFromDef
  , toShapes, toRepr
  , class DataInterchange
  , WithFamilyFn, WithFamilyFn2
  )
  where

import Prelude

import Data.Symbol (reifySymbol)
import Data.List as List
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.SProxy (proxify, reflect')
import Data.Symbol (class IsSymbol)
import Data.Record.Pairs (class Pairs, class Converts)


import Effect.Class (class MonadEffect)
import Control.Monad.Rec.Class (class MonadRec)

-- import Prim.Row (class Cons, class Lacks, class Nub)
import Prim.RowList (RowList, class RowToList)
import Prim.RowList as RL
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
import Noodle.Toolkit.Has as THas
import Noodle.Node.MapsFolds as NM
import Noodle.Node.MapsFolds as NF
import Noodle.Node.MapsFolds.Repr as NR
import Noodle.Node.HoldsNodeState (class IsNodeState)
import Noodle.Toolkit.MapsFolds as TM
import Noodle.Toolkit.MapsFolds as TF
import Noodle.Toolkit.MapsFolds.Repr as TR


import Noodle.Id
import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Wiring (class Wiring)

import Unsafe.Coerce (unsafeCoerce)
import Cli.Components.NodeBox.HasBody (class HasCliBody, class HasCliCustomSize) -- FIXME: must be located somewhere in generic UI


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
    => THas.HasFamilyDef f families' families (Family.Def state is os m)
    => Toolkit gstate families
    -> Family f
    -> m (Node f state is os m)
spawn (Toolkit _ _ tk) fsym =
    Record.get (proxify fsym) tk
        # makeNode
    where
      makeNode (Family.Def (state /\ is /\ os /\ fn)) = Node.make' (family' fsym) state is os fn


unsafeSpawn
    :: forall f (families :: Row Type) (families' ∷ Row Type) gstate state is os m rlfs
     . MonadEffect m
    => ListsFamilies families rlfs
    => THas.HasFamilyDef f families' families (Family.Def state is os m)
    => Toolkit gstate families
    -> Family' f
    -> m (Maybe (Family f /\ Node f state is os m))
unsafeSpawn toolkit@(Toolkit name _ tk) family =
    if List.elem (reflect' family) $ Record.keys tk then
        let (family_ :: Family f) = reifySymbol (reflect' family) unsafeCoerce
        in Just <$> ((/\) family_) <$> (spawn toolkit family_)
    else pure Nothing


unsafeSpawnR
    :: forall f (families :: Row Type) gstate state is os m rlfs
     . MonadEffect m
    => IsSymbol f
    => ListsFamilies families rlfs
    -- => Has.HasFamilyDef' f families' families (Family.Def state is os m) -- it's unsafe in the end
    => Toolkit gstate families
    -> FamilyR
    -> m (Maybe (Node f state is os m))
unsafeSpawnR toolkit@(Toolkit _ name tk) familyR =
    if List.elem (reflect' familyR) $ Record.keys tk then
        RecordU.unsafeGet familyStr tk
            # makeNode
            <#> Just
    else pure Nothing
    where
        familyStr = reflect' familyR
        (family_ :: Family' f) = reifySymbol familyStr unsafeCoerce
        makeNode (state /\ is /\ os /\ fn) = Node.make' family_ state is os fn


unsafeSpawnR'
    :: forall f (families :: Row Type) gstate state is os m rlfs
     . MonadEffect m
    => IsSymbol f
    => ListsFamilies families rlfs
    -- => Has.HasFamilyDef' f families' families (Family.Def state is os m) -- it's unsafe in the end
    => Toolkit gstate families
    -> FamilyR
    -> m (Maybe (Node.HoldsNode' f m))
unsafeSpawnR' toolkit familyR =
    map Node.holdNode' <$> (unsafeSpawnR toolkit familyR :: m (Maybe (Node f state is os m)))


name :: forall gstate families. Toolkit gstate families -> Name
name (Toolkit name _ _) = name


nodeFamilies :: forall gstate rlfs families. ListsFamilies families rlfs => Toolkit gstate families -> List FamilyR
nodeFamilies (Toolkit _ order _) = keysToFamiliesR order (Proxy :: Proxy families)


class DataInterchange (flA :: RowList Type) (flB :: RowList Type)


instance nilNilDataInterchange :: DataInterchange RL.Nil RL.Nil
else instance consNilDataInterchange ::
  ( IsSymbol f
  , DataInterchange tail RL.Nil
  ) => DataInterchange (RL.Cons f (Family.Def state is os m) tail) RL.Nil
else instance nilConsDataInterchange ::
  ( IsSymbol f
  , DataInterchange tail RL.Nil
  ) => DataInterchange RL.Nil (RL.Cons f (Family.Def state is os m) tail)
else instance consConsDataInterchange ::
  ( IsSymbol fA
  , IsSymbol fB
  , RL.RowToList osA osArl
  , RL.RowToList isA isBrl
  , Pairs osArl isBrl
  , DataInterchange tailA (RL.Cons fB (Family.Def stateB isB osB m) tailB)
  , DataInterchange (RL.Cons fA (Family.Def stateA isA osA m) tailA) tailB
  -- , DataInterchange RL.Nil (RL.Cons fB (Family.Def stateB isB osB m) tailB)
  -- , DataInterchange (RL.Cons fA (Family.Def stateA isA osA m) tailA) RL.Nil
  ) => DataInterchange (RL.Cons fA (Family.Def stateA isA osA m) tailA) (RL.Cons fB (Family.Def stateB isB osB m) tailB)


ensureDataInterchangeIn :: forall state families fl. DataInterchange fl fl => RL.RowToList families fl => Toolkit state families -> Unit
ensureDataInterchangeIn _ = unit


-- FIMXE: convert into class?

type WithFamilyFn (x :: Symbol -> Type) (m :: Type -> Type) gstate families instances repr
    =  forall t a
     . Applicative t
    => Wiring m
    => (  forall families' instances' f state (isrl :: RL.RowList Type) (is :: Row Type) (osrl :: RL.RowList Type) (os :: Row Type) repr_is repr_os
        .  THas.HasReprableRenderableNodesOf x gstate families' families instances' instances repr f state isrl is osrl os repr_is repr_os m
        => Family f
        -> Family.Def state is os m
        -> Toolkit gstate families  -- FIXME: toolkit is needed to be passed in the function for the constraints HasFamilyDef/HasInstancesOf to work, maybe only Proxy m is needed?
        -> t a
        )
    -> FamilyR
    -> t (Maybe a)


type WithFamilyFn2 (x :: Symbol -> Type) (m :: Type -> Type) gstate families instances repr
    = forall t a
     . Applicative t
    => Wiring m
    => (  forall familiesA' instancesA' fA stateA (isrlA :: RL.RowList Type) (isA :: Row Type) (osrlA :: RL.RowList Type) (osA :: Row Type) repr_isA repr_osA
                 familiesB' instancesB' fB stateB (isrlB :: RL.RowList Type) (isB :: Row Type) (osrlB :: RL.RowList Type) (osB :: Row Type) repr_isB repr_osB
        .  THas.HasReprableRenderableNodesOf x gstate familiesA' families instancesA' instances repr fA stateA isrlA isA osrlA osA repr_isA repr_osA m
        => THas.HasReprableRenderableNodesOf x gstate familiesB' families instancesB' instances repr fB stateB isrlB isB osrlB osB repr_isB repr_osB m
        => Family fA
        -> Family fB
        -> Family.Def stateA isA osA m
        -> Family.Def stateB isB osB m
        -> Toolkit gstate families
        -> t a
        )
    -> FamilyR
    -> FamilyR
    -> t (Maybe a)


{-
type WithNodeFn (m :: Type -> Type) gstate families instances repr
    =  forall t a
     . Applicative t
    => MonadEffect m
    => (  forall f state fs iis (rli :: RL.RowList Type) (is :: Row Type) (rlo :: RL.RowList Type) (os :: Row Type) repr_is repr_os
        .  HasReprableNodesOf families instances repr f state fs iis rli is rlo os repr_is repr_os m
        => Family f
        -> Family.Def state is os m
        -> Toolkit gstate families  -- FIXME: toolkit is needed to be passed in the function for the constraints HasFamilyDef/HasInstancesOf to work, maybe only Proxy m is needed?
        -> Node f state is os m
        -> t a
        )
    -> FamilyR
    -> t (Maybe a)

-}


{-
class HasWithFamilyFn m gstate families instances repr where
    withFamilyImpl :: WithFamilyFn m gstate families instances repr
-}


{-
inputsFromDef :: forall rli state is os m. HasInputsAt is rli => Family.Def state is os m -> List InputR
inputsFromDef _ = keysToInputsR (Proxy :: Proxy is)


outputsFromDef :: forall rlo state is os m. HasOutputsAt os rlo => Family.Def state is os m -> List OutputR
outputsFromDef _ = keysToOutputsR (Proxy :: Proxy os)
-}