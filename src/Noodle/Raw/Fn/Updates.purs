module Noodle.Raw.Fn.Updates where

import Prelude

import Data.Map (Map)
import Data.Map (toUnfoldable) as Map
import Data.Bifunctor (lmap)
import Data.Tuple (uncurry)

import Noodle.Id (InletR, OutletR, NodeR)
import Noodle.Id (inletRName, outletRName) as Id
import Noodle.Fn.Generic.Updates as Generic

import Noodle.Fn.ToFn (Fn)
import Noodle.Fn.ToFn (Argument, Output, arg, out) as Fn


type Update state repr = Generic.Update state (Map InletR repr) (Map OutletR repr)
type MergedUpdate state repr = Generic.MergedUpdate state (Map InletR repr) (Map OutletR repr)


toFn :: forall state repr. NodeR -> MergedUpdate state repr -> Fn repr repr
toFn nodeR = Generic.toFn nodeR inletsToArgs outletsToArgs
  where
    inletsToArgs  :: Map InletR repr -> Array (Fn.Argument repr)
    inletsToArgs   = Map.toUnfoldable >>> map (lmap Id.inletRName >>> uncurry Fn.arg)
    outletsToArgs :: Map OutletR repr -> Array (Fn.Output repr)
    outletsToArgs  = Map.toUnfoldable >>> map (lmap Id.outletRName >>> uncurry Fn.out)