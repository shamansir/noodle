module Noodle.Raw.Fn.Updates where

import Prelude

import Data.Map (Map)
import Data.Map (toUnfoldable) as Map
import Data.Bifunctor (lmap,bimap)
import Data.Tuple (uncurry)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Id (InletR, OutletR, NodeR)
import Noodle.Id (inletRName, outletRName) as Id
import Noodle.Fn.Generic.Updates as Generic

import Noodle.Fn.Signature (Sig)
import Noodle.Fn.Signature (Argument, Output, arg, out) as Sig

import Noodle.Repr.ValueInChannel (ValueInChannel)


type Update state repr              = Generic.Update       state (Map InletR (ValueInChannel repr))          (Map OutletR (ValueInChannel repr))
type MergedUpdate state repr        = Generic.MergedUpdate state (Map InletR (ValueInChannel repr))          (Map OutletR (ValueInChannel repr))
type OrderedMergedUpdate state repr = Generic.MergedUpdate state (Map (Int /\ InletR) (ValueInChannel repr)) (Map (Int /\ OutletR) (ValueInChannel repr))


toSignature :: forall state repr. NodeR -> MergedUpdate state repr -> Sig (ValueInChannel repr) (ValueInChannel repr)
toSignature nodeR = Generic.toSignature nodeR inletsToArgs outletsToArgs
  where
    inletsToArgs  :: Map InletR (ValueInChannel repr) -> Array (Sig.Argument (ValueInChannel repr))
    inletsToArgs   = Map.toUnfoldable >>> map (lmap Id.inletRName >>> uncurry Sig.arg)
    outletsToArgs :: Map OutletR (ValueInChannel repr) -> Array (Sig.Output (ValueInChannel repr))
    outletsToArgs  = Map.toUnfoldable >>> map (lmap Id.outletRName >>> uncurry Sig.out)



orderedToSignature :: forall state repr. NodeR -> OrderedMergedUpdate state repr -> Sig (ValueInChannel repr) (ValueInChannel repr)
orderedToSignature nodeR = Generic.toSignature nodeR inletsToArgs outletsToArgs
  where
    inletsToArgs  :: Map (Int /\ InletR) (ValueInChannel repr) -> Array (Sig.Argument (ValueInChannel repr))
    inletsToArgs   = Map.toUnfoldable >>> map (lmap (Tuple.snd >>> Id.inletRName) >>> uncurry Sig.arg)
    outletsToArgs :: Map (Int /\ OutletR) (ValueInChannel repr) -> Array (Sig.Output (ValueInChannel repr))
    outletsToArgs  = Map.toUnfoldable >>> map (lmap (Tuple.snd >>> Id.outletRName) >>> uncurry Sig.out)