module Rpd.Test.Spec.RpdFileParser
    ( spec ) where


import Prelude

import Data.Either (either)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Map (empty) as Map
import Data.String as String
import Data.Tuple.Nested ((/\))

import Effect.Aff (Aff)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)

import Rpd.Path as R
import Rpd.Process (ProcessF(..)) as R
-- import Rpd.Command (Command)
-- import Rpd.Command as Cmd
import Rpd.Toolkit as T
import Rpd.Parser.RpdFile (parse, RpdFileCommand, RpdFile)
import Rpd.Parser.RpdFile (RpdFileCommand(..)) as Cmd


data Data = Foo | Bar


data Channel = Whatever


data Node = FooNode


instance exampleChannels :: T.Channels Data Channel where
    default _ = Foo
    accept _ _ = true
    adapt _ = identity


instance showData :: Show Data where
    show Foo = "Foo"
    show Bar = "Bar"


instance eqData :: Eq Data where
    eq Foo Foo = true
    eq Bar Bar = true
    eq _ _ = false


toolkit :: T.Toolkit Data Channel Node
toolkit =
    T.Toolkit
        (T.ToolkitName "test")
        nodes
    where
        nodes FooNode = fooDef



fooDef :: T.NodeDef Data Channel
fooDef =
    T.NodeDef
        { inlets : T.inlets [ "test" /\ Whatever ]
        , outlets : T.outlets [ "test" /\ Whatever ]
        , process : R.Withhold
        }


spec :: Spec Unit
spec =
  describe "parsing commands" $ do

    it "parses single command" do
      _ <- "node test/foo a\n" `parsesAs`
                [ Cmd.AddNode (R.toNode "test" "foo") (T.NodeDefAlias "a")
                ]
      pure unit

    it "parses a bunch of commands" $ do
      let bunchOfCommands =
            String.joinWith "\n"
                [ "patch test"
                , "node test/foo a"
                , "inlet test/foo/i1 wave"
                , "inlet test/foo/i2 random"
                , "outlet test/foo/o color"
                , "node test/bar b"
                , "inlet test/bar/the-inlet color"
                , "connect test/foo/o test/bar/the-inlet"
                ] <> "\n"
      _ <- bunchOfCommands `parsesAs`
                [ Cmd.AddPatch (R.toPatch "test")
                , Cmd.AddNode (R.toNode "test" "foo") (T.NodeDefAlias "a")
                , Cmd.AddInlet (R.toInlet "test" "foo" "i1") (T.ChannelDefAlias "wave")
                , Cmd.AddInlet (R.toInlet "test" "foo" "i2") (T.ChannelDefAlias "random")
                , Cmd.AddOutlet (R.toOutlet "test" "foo" "o") (T.ChannelDefAlias "color")
                , Cmd.AddNode (R.toNode "test" "bar") (T.NodeDefAlias "b")
                , Cmd.AddInlet (R.toInlet "test" "foo" "the-inlet") (T.ChannelDefAlias "color")
                , Cmd.Connect (R.toOutlet "test" "foo" "o")  (R.toInlet "test" "foo" "the-inlet")
                ]
      pure unit

    it "is ok with dashes in names" $ do
      let bunchOfCommands =
            String.joinWith "\n"
                [ "patch te-st"
                , "node te-st/fo-o a-node"
                , "inlet te-st/fo-o/inlet-1 ha-ha"
                , "outlet te-st/fo-o/outlet-1 well-well"
                , "connect te-st/fo-o/outlet-1 te-st/fo-o/inlet-1"
                    -- a loop but who cares in this case?
                ] <> "\n"
      _ <- bunchOfCommands `parsesAs`
                [ Cmd.AddPatch (R.toPatch "te-st")
                , Cmd.AddNode (R.toNode "te-st" "fo-o") (T.NodeDefAlias "a-node")
                , Cmd.AddInlet
                    (R.toInlet "te-st" "fo-o" "inlet-1")
                    (T.ChannelDefAlias "ha-ha")
                , Cmd.AddOutlet
                    (R.toOutlet "te-st" "fo-o" "outlet-1")
                    (T.ChannelDefAlias "well-well")
                , Cmd.Connect
                    (R.toOutlet "te-st" "fo-o" "outlet-1")
                    (R.toInlet "te-st" "fo-o" "inlet-1")
                ]
      pure unit

    it "is ok with underscores in names" $ do
      let bunchOfCommands =
            String.joinWith "\n"
                [ "patch te_st"
                , "node te_st/fo_o a_node"
                , "inlet te_st/fo_o/inlet_1 ha_ha"
                , "outlet te_st/fo_o/outlet_1 well_well"
                , "connect te_st/fo_o/outlet_1 te_st/fo_o/inlet_1"
                    -- a loop but who cares in this case?
                ] <> "\n"
      _ <- bunchOfCommands `parsesAs`
                [ Cmd.AddPatch (R.toPatch "te_st")
                , Cmd.AddNode (R.toNode "te_st" "fo_o") (T.NodeDefAlias "a_node")
                , Cmd.AddInlet
                    (R.toInlet "te_st" "fo_o" "inlet_1")
                    (T.ChannelDefAlias "ha_ha")
                , Cmd.AddOutlet
                    (R.toOutlet "te_st" "fo_o" "outlet_1")
                    (T.ChannelDefAlias "well_well")
                , Cmd.Connect
                    (R.toOutlet "te_st" "fo_o" "outlet_1")
                    (R.toInlet "te_st" "fo_o" "inlet_1")
                ]
      pure unit


parsesAs :: String -> Array RpdFileCommand  -> Aff Unit
parsesAs input expected =
    (toolkit
        # parse input)
    -- toolkit
    --     # parse input
        # either
            (fail <<< show)
            (shouldEqual $ List.fromFoldable expected)


-- parsesAs' :: String -> StringCommand -> Aff Unit
-- parsesAs' input expected =
--     parse' input
--         # either
--             (fail <<< show)
--             (shouldEqual expected)

