module Test.Main where

import Prelude (Unit, ($), discard)

import Effect (Effect)
import Effect.Aff (launchAff_)

import Test.Spec (describe, describeOnly)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

-- import Test.Nodes (spec) as Nodes
import Test.Node2 (spec) as Node2
import Test.Fn (spec) as Fn
import Test.Toolkit3 (spec) as Toolkit3
-- import Test.Toolkit2 (spec) as Toolkit2
import Test.Protocol2 (spec) as Protocol2
import Test.Patch4 (spec) as Patch4
import Test.Flex (spec) as Flex
import Test.UniqueHash (spec) as UniqueHash
import Test.Generating (spec) as Generating
import Test.NdfParsing (spec) as NdfParsing
import Test.SOrder (spec) as SOrder
import Test.NodeKey (spec) as NodeKey


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "UniqueHash"
    UniqueHash.spec
  -- describe "Fn"
  --   Fn.spec
  describe "Node2"
    Node2.spec
  -- describe "Nodes"
  --   Nodes.spec
  -- describe "Toolkit"
  --   Toolkit.spec
  -- describe "Toolkit2"
  --   Toolkit2.spec
  describe "Patch4"
    Patch4.spec
  describe "Toolkit3"
    Toolkit3.spec
  describe "Protocol2"
    Protocol2.spec
  describe "NDF Parsing"
    NdfParsing.spec
  describe "Parsing & Generating Toolkits"
    Generating.spec
  describe "Flex"
    Flex.spec
  describe "SOrder"
    SOrder.spec
  describe "NodeKey"
    NodeKey.spec
