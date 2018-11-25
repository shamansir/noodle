module RpdTest.CommandParser
    ( spec ) where


import Prelude

import Data.Either (either)
import Data.List as List
import Data.Maybe

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)

import Rpd (init, run') as R
import Rpd.API (Rpd) as R
import Rpd.Def as R
import Rpd.Path as R
import Rpd.Network (Network) as R
import Rpd.Command (Command, StringCommand)
import Rpd.Command as Cmd
import Rpd.Toolkit as T
import Rpd.CommandParser (parse, parse')


data Data = Foo | Bar


toolkit :: T.Toolkit Data
toolkit =
    { id : "test"
    , nodeDefs : T.singleDef "foo" fooDef
    , channelDefs : T.noDefs
    }


fooDef :: R.NodeDef Data
fooDef =
    { name : "foo"
    , inletDefs : R.noDefs
    , outletDefs : R.noDefs
    , process : R.FlowThrough
    }


spec :: Spec Unit
spec =
  describe "parsing commands" do
    -- it "parses commands" do
    --   _ <- "node 0 test/foo" `parsesAs` Cmd.AddNode (R.PatchId 0) fooDef
    --   pure unit
    it "parses string commands" do
      _ <- "node 0 foo" `parsesAs'` Cmd.AddNode' (R.PatchId 0) "foo"
      pure unit


-- parsesAs :: forall d. Show d => String -> Command Data -> Aff Unit
-- parsesAs input expected =
--     ?wh
--         # parse input
--         # either
--             (fail <<< show)
--             (maybe'
--                 (const $ fail "parsing failed")
--                 (shouldEqual expected))


parsesAs' :: String -> StringCommand -> Aff Unit
parsesAs' input expected =
    parse' input
        # either
            (fail <<< show)
            (shouldEqual expected)

