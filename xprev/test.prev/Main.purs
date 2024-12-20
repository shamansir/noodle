module Test.Main where

import Prelude (Unit, ($), discard)

import Effect (Effect)
import Effect.Aff (launchAff_)

import Test.Spec (describe, describeOnly, pending')
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

-- import Test.Nodes (spec) as Nodes
import Test.Node (spec) as Node
import Test.Fn (spec) as Fn
import Test.Toolkit (spec) as Toolkit
import Test.Protocol (spec) as Protocol
import Test.Patch (spec) as Patch
import Test.Flex (spec) as Flex
import Test.UniqueHash (spec) as UniqueHash
import Test.Generating (spec) as Generating
import Test.NdfParsing (spec) as NdfParsing
import Test.SOrder (spec) as SOrder
import Test.NodeKey (spec) as NodeKey
import Test.JsExprParsing (spec) as JsExprParsing
import Test.WrapReprParsing (spec) as WrapReprParsing
import Test.JsExprParsing.Wrap (spec) as JsExprParsingWrap


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "UniqueHash"
    UniqueHash.spec
  describe "Fn"
    Fn.spec
  describe "Node"
    Node.spec
  -- describe "Nodes"
  --   Nodes.spec
  describe "Toolkit"
    Toolkit.spec
  describe "Patch"
    Patch.spec
  describe "Toolkit"
    Toolkit.spec
  describe "Protocol"
    Protocol.spec
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
  describe "Expressions parsing"
    JsExprParsing.spec
  describe "Expressions parsing (Wrap)"
    JsExprParsingWrap.spec
  describe "Hydra Serialization"
    WrapReprParsing.spec
