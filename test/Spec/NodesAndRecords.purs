module Test.Spec.NodesAndRecords where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Noodle.Fn.Shape (Shape(..))
import Noodle.Fn.Shape (_reflect) as Shape
import Noodle.Raw.Id (inletR, outletR)
import Noodle.Id (Temperament(..), inletRName, outletRName)
import Noodle.Raw.Fn.Shape (inlets, outlets, tagAs) as RawShape
import Example.Toolkit.Minimal.Node.SampleHC as SampleHC


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