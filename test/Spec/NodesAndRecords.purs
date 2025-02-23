module Test.Spec.NodesAndRecords where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Data.Symbol (reflectSymbol)
import Noodle.Repr.ValueInChannel (accept, toMaybe) as ViC
import Noodle.Fn.Shape (Shape(..))
import Noodle.Fn.Shape (_reflect) as Shape
import Noodle.Raw.Id (inletR, outletR, familyR)
import Noodle.Id (Temperament(..), inletRName, outletRName)
import Noodle.Raw.Fn.Shape (inlets, outlets, make, tagAs) as RawShape
import Example.Toolkit.Minimal.Node.SampleHC as SampleHC
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.Tagged (class Tagged)
import Noodle.Repr.Tagged (Path, inlet) as Tag
import Noodle.Repr.Tagged (tag) as Repr


import Noodle.Raw.FromToRec as RR

import Example.Toolkit.Minimal.Repr (MinimalVRepr(..))


spec :: Spec Unit
spec = do

    describe "creating maps from reprs" $ do

        it "properly instantiates / reflects shape" $ do
            let
                inletToTag =
                    inletRName >>> case _ of
                        "foo" -> "Int"
                        "c" -> "Str"
                        "bar" -> "Int"
                        _ -> "ERR"
                    >>> RawShape.tagAs
                outletToTag =
                    outletRName >>> case _ of
                        "foo" -> "Int"
                        "bar" -> "Int"
                        _ -> "ERR"
                    >>> RawShape.tagAs
                rawShape =
                    Shape._reflect inletToTag outletToTag (Shape :: SampleHC.Shape)
            RawShape.inlets rawShape `shouldEqual`
                [ { name : inletR "foo", order : 0, temp : Hot,  tag : RawShape.tagAs "Int" }
                , { name : inletR "c"  , order : 1, temp : Hot,  tag : RawShape.tagAs "Str" }
                , { name : inletR "bar", order : 2, temp : Cold, tag : RawShape.tagAs "Int"}
                ]
            RawShape.outlets rawShape `shouldEqual`
                [ { name : outletR "foo", order : 0, tag : RawShape.tagAs "Int" }
                , { name : outletR "bar", order : 1, tag : RawShape.tagAs "Int" }
                ]