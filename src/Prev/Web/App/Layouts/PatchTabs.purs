module Prev.Layouts.PatchTabs where

import Prelude

import Data.Vec2 ((<+>))
import Data.Set (Set)
import Data.Set as Set
import Data.Array ((:))
import Data.Ord (class Ord, compare)
import Data.Ordering (Ordering(..))

import Prev.Layout.Strip (Strip)
import Prev.Layout.Strip (make) as Strip
import Prev.Layout.Flex as F
import Prev.Layout.Flex.Rule (Rule)
import Prev.Layout.Flex.Rule as R
import Data.Tuple.Nested ((/\), type (/\))

import Prev.Noodle.Patch (Patch)
import Prev.Noodle.Patch (Id) as Patch


type Layout = Strip TabOrAdd


data TabOrAdd
    = PatchTab Patch.Id
    | Add


instance eqTabOrAdd :: Eq TabOrAdd where
    eq (PatchTab _) Add = false
    eq Add (PatchTab _) = false
    eq Add Add = true
    eq (PatchTab idA) (PatchTab idB) = eq idA idB


instance ordTabOrAdd :: Ord TabOrAdd where
    compare (PatchTab _) Add = GT
    compare Add (PatchTab _) = LT
    compare Add Add = EQ
    compare (PatchTab idA) (PatchTab idB) = compare idA idB


layout :: Number -> Array Patch.Id -> Layout
layout maxWidth patches =
    Strip.make ((50.0 <+> 20.0) /\ (5.0 <+> 5.0)) maxWidth $ Set.fromFoldable $ Add : (PatchTab <$> patches)
