module Test.Spec.CommandInput where

import Prelude

import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil)


import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn, fail)

import Noodle.Id (familyR, family, FamilyR(..), toolkitR) as Id
import Noodle.Raw.Node (shape) as RawNode
import Noodle.Raw.Fn.Shape as RawShape
import Noodle.Raw.FromToRec as RR
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit (empty, register) as Toolkit
import Noodle.Fn.Shape.Temperament (Temperament(..)) as Temp

import Example.Toolkit.Minimal.ChRepr (MinimalVRepr(..))
import Example.Toolkit.Minimal.StRepr (MinimalStRepr(..))
import Example.Toolkit.Minimal.Node.Concat as Concat
import Example.Toolkit.Minimal.Node.Sum as Sum
import Example.Toolkit.Minimal.Toolkit (Toolkit, toolkit, MINIMAL,  minimalTk) as Minimal

import Noodle.Text.NdfFile.Command.FromInput as FI

-- type MinimalTk fs m = Toolkit My.MINIMAL fs MinimalStRepr MinimalVRepr m


spec :: Spec Unit
spec = do

    describe "command input" $ do

        it "asking for a node to be spawned from toolkit by family works" $ do
            command <- FI.tryExecute Minimal.toolkit "concat"
            case command of
                FI.FromFamily familyR rawNode -> do
                    Id.family familyR `shouldEqual` "concat"
                    RawNode.shape rawNode `shouldEqual`
                        (RawShape.make
                            { inlets :
                                [ { name : RawShape.unsafeInletR "left" , order : 0, tag : RawShape.tagAs "Str", temp : Temp.Hot }
                                , { name : RawShape.unsafeInletR "right", order : 1, tag : RawShape.tagAs "Str", temp : Temp.Hot }
                                ]
                            , outlets :
                                [ { name : RawShape.unsafeOutletR "out", order : 0, tag : RawShape.tagAs "Str" }
                                , { name : RawShape.unsafeOutletR "len", order : 1, tag : RawShape.tagAs "Int" }
                                ]
                            }
                        )
                _ -> fail $ "`concat` command didn't produce the expected node"

        it "asking for a node with specific shape to spawned works (example 1, empty node)" $ do
            command <- FI.tryExecute Minimal.toolkit ":: <> => <>"
            case command of
                FI.CustomNode sig rawNode -> do
                    RawNode.shape rawNode `shouldEqual`
                        (RawShape.make
                            { inlets :
                                [ ]
                            , outlets :
                                [ ]
                            }
                        )
                _ -> fail $ "`:: <> => <>` command didn't produce the expected node"


        it "asking for a node with specific shape to spawned works (example 2, node with three inlets)" $ do
            command <- FI.tryExecute Minimal.toolkit ":: <a -> b -> c> => <>"
            case command of
                FI.CustomNode sig rawNode -> do
                    RawNode.shape rawNode `shouldEqual`
                        (RawShape.make
                            { inlets :
                                [ { name : RawShape.unsafeInletR "a", order : 0, tag : RawShape.tagAs "None", temp : Temp.Hot }
                                , { name : RawShape.unsafeInletR "b", order : 1, tag : RawShape.tagAs "None", temp : Temp.Hot }
                                , { name : RawShape.unsafeInletR "c", order : 2, tag : RawShape.tagAs "None", temp : Temp.Hot }
                                ]
                            , outlets :
                                [ ]
                            }
                        )
                _ -> fail $ "`:: <a -> b -> c>` command didn't produce the expected node"


        it "asking for a node with specific shape to spawned works (example 3, node with two outlets)" $ do
            command <- FI.tryExecute Minimal.toolkit ":: <> => <foo -> bar>"
            case command of
                FI.CustomNode sig rawNode -> do
                    RawNode.shape rawNode `shouldEqual`
                        (RawShape.make
                            { inlets :
                                [ ]
                            , outlets :
                                [ { name : RawShape.unsafeOutletR "foo", order : 0, tag : RawShape.tagAs "None" }
                                , { name : RawShape.unsafeOutletR "bar", order : 1, tag : RawShape.tagAs "None" }
                                ]
                            }
                        )
                _ -> fail $ "`:: <> => <foo -> bar>` command didn't produce the expected node"

        it "asking for a node with specific shape to spawned works (example 4, node with inlets and outlets)" $ do
            command <- FI.tryExecute Minimal.toolkit ":: <a -> b -> c> => <foo -> bar>"
            case command of
                FI.CustomNode sig rawNode -> do
                    RawNode.shape rawNode `shouldEqual`
                        (RawShape.make
                            { inlets :
                                [ { name : RawShape.unsafeInletR "a", order : 0, tag : RawShape.tagAs "None", temp : Temp.Hot }
                                , { name : RawShape.unsafeInletR "b", order : 1, tag : RawShape.tagAs "None", temp : Temp.Hot }
                                , { name : RawShape.unsafeInletR "c", order : 2, tag : RawShape.tagAs "None", temp : Temp.Hot }
                                ]
                            , outlets :
                                [ { name : RawShape.unsafeOutletR "foo", order : 0, tag : RawShape.tagAs "None" }
                                , { name : RawShape.unsafeOutletR "bar", order : 1, tag : RawShape.tagAs "None" }
                                ]
                            }
                        )
                _ -> fail $ "`:: <a -> b -> c> => <foo -> bar>` command didn't produce the expected node"


        it "asking for a node with specific shape to spawned works (example 5, specifying types)" $ do
            command <- FI.tryExecute Minimal.toolkit ":: <a:String -> b:Int -> c> => <foo -> bar:Unit>"
            case command of
                FI.CustomNode sig rawNode -> do
                    RawNode.shape rawNode `shouldEqual`
                        (RawShape.make
                            { inlets :
                                [ { name : RawShape.unsafeInletR "a", order : 0, tag : RawShape.tagAs "Str", temp : Temp.Hot }
                                , { name : RawShape.unsafeInletR "b", order : 1, tag : RawShape.tagAs "Int", temp : Temp.Hot }
                                , { name : RawShape.unsafeInletR "c", order : 2, tag : RawShape.tagAs "None", temp : Temp.Hot }
                                ]
                            , outlets :
                                [ { name : RawShape.unsafeOutletR "foo", order : 0, tag : RawShape.tagAs "None" }
                                , { name : RawShape.unsafeOutletR "bar", order : 1, tag : RawShape.tagAs "Unit" }
                                ]
                            }
                        )
                _ -> fail $ "`:: <a:String -> b:Int -> c> => <foo -> bar:Unit>` command didn't produce the expected node"


        it "asking for a node with specific shape to spawned works (example 6, specifying types and default values)" $ do
            command <- FI.tryExecute Minimal.toolkit ":: <a:String {\"1\"} -> b:Int {2} -> c> => <foo -> bar:Unit {unit}>"
            case command of
                FI.CustomNode sig rawNode -> do
                    RawNode.shape rawNode `shouldEqual`
                        (RawShape.make
                            { inlets :
                                [ { name : RawShape.unsafeInletR "a", order : 0, tag : RawShape.tagAs "Str", temp : Temp.Hot }
                                , { name : RawShape.unsafeInletR "b", order : 1, tag : RawShape.tagAs "Int", temp : Temp.Hot }
                                , { name : RawShape.unsafeInletR "c", order : 2, tag : RawShape.tagAs "None", temp : Temp.Hot }
                                ]
                            , outlets :
                                [ { name : RawShape.unsafeOutletR "foo", order : 0, tag : RawShape.tagAs "None" }
                                , { name : RawShape.unsafeOutletR "bar", order : 1, tag : RawShape.tagAs "Unit" }
                                ]
                            }
                        )
                    -- TODO: test default values applied
                _ -> fail $ "`:: <a:String {\"1\"} -> b:Int {2} -> c> => <foo -> bar:Unit {unit}>` command didn't produce the expected node"