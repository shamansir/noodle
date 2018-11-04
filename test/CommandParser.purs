module RpdTest.CommandParser
    ( spec ) where


import Prelude

import Data.Either (either)

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)

import Rpd (init, run') as R
import Rpd.API (Rpd) as R
import Rpd.Network (Network) as R
import Rpd.Command (Command)
import Rpd.Command as Cmd

import Rpd.Renderer.Terminal.CommandParser (parse)

spec :: Spec Unit
spec =
  describe "parsing commands" do
    it "parses commands" do
      _ <- "4" `parsesAs` Cmd.Bang
      pure unit


parsesAs :: String -> Command -> Aff Unit
parsesAs input expected =
    parse input
        # either
            (fail <<< show)
            (shouldEqual expected)
