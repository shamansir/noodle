module Test.Main where

import Prelude (Unit, ($), discard)

import Effect (Effect)
import Effect.Aff (launchAff_)

import Test.Spec (describe, describeOnly, pending')
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

-- import Test.Nodes (spec) as Nodes
import Test.Spec.Fn (spec) as Fn
import Test.Spec.UniqueHash (spec) as UniqueHash
import Test.Spec.Repr (spec) as Repr
import Test.Spec.Node (spec) as Node
import Test.Spec.Toolkit (spec) as Toolkit
import Test.Spec.Patch (spec) as Patch
import Test.Spec.NdfFile (spec) as NdfFile
import Test.Spec.NdfCodegen (spec) as NdfCodegen
import Test.Spec.HydraReprParsing (spec) as HydraReprParsing


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "UniqueHash"
    UniqueHash.spec
  describe "Repr"
    Repr.spec
  describe "Fn"
    Fn.spec
  describe "Node"
    Node.spec
  describe "Toolkit"
    Toolkit.spec
  describe "Patch"
    Patch.spec
  describe "NDF File"
    NdfFile.spec
  -- describe "Codegen from NDF Definitions" -- FIXME: enable back
  --   NdfCodegen.spec
  describe "Hydra Repr parsing"
    HydraReprParsing.spec