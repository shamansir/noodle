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
. 20 30 pi-0
"""


expectedNdf :: NdfFile
expectedNdf =
    NdfFile.from "hydra" 0.1
        [ C.MakeNode (C.family "osc") (C.coord 40) (C.coord 60) (C.nodeId "osc-0")
        , C.MakeNode (C.family "osc") (C.coord 40) (C.coord 60) (C.nodeId "osc-1")
        , C.MakeNode (C.family "pi") (C.coord 20) (C.coord 20) (C.nodeId "pi-0")
        , C.MakeNode (C.family "number") (C.coord 40) (C.coord 40) (C.nodeId "num-0")
        , C.Connect (C.nodeId "pi-0") (C.outputIndex 0) (C.nodeId "osc-0") (C.inputIndex 0)
        , C.Connect (C.nodeId "num-0") (C.outputIndex 0) (C.nodeId "osc-0") (C.inputIndex 1)
        , C.Send (C.nodeId "osc-0") (C.inputIndex 0) (C.encodedValue "N 20.0")
        , C.SendO (C.nodeId "num-0") (C.outputIndex 0) (C.encodedValue "N 40.0")
        , C.Connect (C.nodeId "pi-0") (C.outputAlias "foo") (C.nodeId "osc-0") (C.inputAlias "bar")
        , C.Send (C.nodeId "osc-0") (C.inputAlias "foo") (C.encodedValue "N 20.0")
        , C.SendO (C.nodeId "num-0") (C.outputAlias "bar") (C.encodedValue "N 40.0")
        , C.Move (C.nodeId "pi-0") (C.coord 20) (C.coord 30)
        ]


spec :: Spec Unit
spec = do

  describe "Parses NDF File properly" $ do

    it "parsing works" $
      parses sampleNdfText expectedNdf NdfFile.parser
