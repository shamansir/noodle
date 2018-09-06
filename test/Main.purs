module Test.Main where

import Prelude

import Effect (Effect)

import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import RpdTest.Main (spec)

main :: Effect Unit
main = run [consoleReporter] spec

