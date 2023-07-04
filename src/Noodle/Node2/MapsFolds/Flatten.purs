module Noodle.Node2.MapsFolds.Flatten where

import Prelude

import Data.UniqueHash (UniqueHash)
-- import Data.Array ()
import Data.Map (Map)
import Data.Map (toUnfoldable, lookup) as Map
import Data.Maybe (Maybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (lmap)
import Data.SProxy (reflect', proxify)
import Data.Symbol (class IsSymbol)

import Prim.Row (class Cons)
import Record as Record

import Noodle.Id (FamilyR, Input, InputR, NodeId, NodeIdR, Output, OutputR, splitR, reflectNodeIdR)
import Noodle.Fn2.Protocol (ChangeFocus)


type NodeLineRec f repr repr_is repr_os =
    ChangeFocus /\ NodeId f /\ repr /\ Record repr_is /\ Record repr_os

type NodeLineMap repr =
    ChangeFocus /\ NodeIdR /\ repr /\ Map InputR repr /\ Map OutputR repr


flatten :: forall repr. NodeLineMap repr -> (FamilyR /\ UniqueHash) /\ repr /\ Array (InputR /\ repr) /\ Array (OutputR /\ repr)
flatten (_ /\ nodeId /\ repr /\ inputsMap /\ outputsMap) = splitR nodeId /\ repr /\ Map.toUnfoldable inputsMap /\ Map.toUnfoldable outputsMap


flatten' :: forall repr. NodeLineMap repr -> (String /\ String) /\ repr /\ Array (String /\ repr) /\ Array (String /\ repr)
flatten' (_ /\ nodeId /\ repr /\ inputsMap /\ outputsMap) = reflectNodeIdR nodeId /\ repr /\ (lmap reflect' <$> Map.toUnfoldable inputsMap) /\ (lmap reflect' <$> Map.toUnfoldable outputsMap)


flatten'' :: forall repr. NodeLineMap repr -> String /\ repr /\ Array (String /\ repr) /\ Array (String /\ repr)
flatten'' = flatten' >>> lmap fst


getStateFromRec :: forall f repr repr_is repr_os. NodeLineRec f repr repr_is repr_os -> repr
getStateFromRec (_ /\ _ /\ state /\ _ /\ _) = state


getStateFromMap :: forall repr. NodeLineMap repr -> repr
getStateFromMap (_ /\ _ /\ state /\ _ /\ _) = state


getInputFromRec :: forall f repr repr_is repr_os i trash. IsSymbol i => Cons i repr trash repr_is => NodeLineRec f repr repr_is repr_os -> Input i -> repr
getInputFromRec (_ /\ _ /\ _ /\ inputs /\ _) input = Record.get (proxify input) inputs


getOutputFromRec :: forall f repr repr_is repr_os o trash. IsSymbol o => Cons o repr trash repr_os => NodeLineRec f repr repr_is repr_os -> Output o -> repr
getOutputFromRec (_ /\ _ /\ _ /\ _ /\ outputs) output = Record.get (proxify output) outputs


getInputFromMap :: forall repr. NodeLineMap repr -> InputR -> Maybe repr
getInputFromMap (_ /\ _ /\ _ /\ inputs /\ _) input = Map.lookup input inputs


getOutputFromMap :: forall repr. NodeLineMap repr -> OutputR -> Maybe repr
getOutputFromMap (_ /\ _ /\ _ /\ _ /\ outputs) output = Map.lookup output outputs
