module RpdTest.CommandParser
    ( spec ) where


import Prelude

import Data.Either (either)
import Data.List (List)
import Data.List as List
import Data.Maybe
import Data.Tuple.Nested ((/\))

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)

import Rpd (init, run') as R
import Rpd.API (Rpd) as R
import Rpd.Path as R
import Rpd.Process as R
import Rpd.Network (Network) as R
-- import Rpd.Command (Command)
-- import Rpd.Command as Cmd
import Rpd.Toolkit as T
import Rpd.Parser.RpdFile (parse, RpdFileCommand, RpdFile)
import Rpd.Parser.RpdFile (RpdFileCommand(..)) as Cmd


data Data = Foo | Bar


data Channel = Whatever


instance exampleChannel :: T.Channel Channel Data where
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


toolkit :: T.Toolkit Channel Data
toolkit =
    T.Toolkit
        { name : T.ToolkitName "test"
        , nodes : T.nodes [ "foo" /\ fooDef ]
        }


fooDef :: T.NodeDef Channel Data
fooDef =
    T.NodeDef
        { inlets : T.inlets [ "test" /\ Whatever ]
        , outlets : T.outlets [ "test" /\ Whatever ]
        , process : R.Withhold
        }


spec :: Spec Unit
spec =
  describe "parsing commands" do
    it "parses commands" do
      _ <- "node test/foo a\n" `parsesAs`
                [ Cmd.AddNode (R.toNode "test" "foo") (T.NodeDefAlias "a")
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

