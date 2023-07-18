module Test.NdfParsing where

import Prelude

import Test.Spec (Spec, describe, it)

import Test.Generating (parses)

import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile.Command as C
import Noodle.Text.NdfFile (from) as NdfFile
import Noodle.Text.NdfFile.Parser (parser) as NdfFile


sampleNdfText :: String
sampleNdfText =
    """hydra 0.1
osc 40 60 osc-0
osc 40 60 osc-1
pi 20 20 pi-0
number 40 40 num-0
<> pi-0 0 osc-0 0
<> num-0 0 osc-0 1
-> osc-0 0 N 20.0
~> num-0 0 N 40.0
<> pi-0 foo osc-0 bar
-> osc-0 foo N 20.0
~> num-0 bar N 40.0
"""


expectedNdf :: NdfFile
expectedNdf =
    NdfFile.from "hydra" 0.1
        [ C.MakeNode "osc" 40 60 "osc-0"
        , C.MakeNode "osc" 40 60 "osc-1"
        , C.MakeNode "pi" 20 20 "pi-0"
        , C.MakeNode "number" 40 40 "num-0"
        , C.Connect "pi-0" 0 "osc-0" 0
        , C.Connect "num-0" 0 "osc-0" 1
        , C.Send "osc-0" 0 "N 20.0"
        , C.SendO "num-0" 0 "N 40.0"
        , C.Connect_ "pi-0" "foo" "osc-0" "bar"
        , C.Send_ "osc-0" "foo" "N 20.0"
        , C.SendO_ "num-0" "bar" "N 40.0"
        ]


spec :: Spec Unit
spec = do

  describe "Parses NDF File properly" $ do

    it "parsing works" $
      parses sampleNdfText expectedNdf NdfFile.parser
