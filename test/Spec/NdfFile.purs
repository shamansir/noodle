module Test.Spec.NdfFile where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Util.Parsing (parses)

import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile.Command as C
import Noodle.Text.NdfFile (from, from_, toNdfCode) as NdfFile
import Noodle.Text.NdfFile.Parser (parser) as NdfFile


sampleNdf_0_1_Text :: String
sampleNdf_0_1_Text =
    """hydra 0.1
# test example
osc 40 60 osc-0
osc 40 60 osc-1
pi 20 20 pi-0
number 40 40 num-0
# connect pi to osc
<> pi-0 0 osc-0 0
<> num-0 0 osc-0 1
-> osc-0 0 N 20.0
~> num-0 0 N 40.0
<> pi-0 foo osc-0 bar
-> osc-0 foo N 20.0
~> num-0 bar N 40.0
. 20 30 pi-0
"""


expected_0_1_Ndf :: NdfFile
expected_0_1_Ndf =
    NdfFile.from_ { toolkit : "hydra", toolkitVersion : 0.1, ndfVersion : 0.1 }
        [ C.Comment "test example"
        , C.MakeNode (C.family "osc") (C.coord 40) (C.coord 60) (C.nodeId "osc-0")
        , C.MakeNode (C.family "osc") (C.coord 40) (C.coord 60) (C.nodeId "osc-1")
        , C.MakeNode (C.family "pi") (C.coord 20) (C.coord 20) (C.nodeId "pi-0")
        , C.MakeNode (C.family "number") (C.coord 40) (C.coord 40) (C.nodeId "num-0")
        , C.Comment "connect pi to osc"
        , C.Connect (C.nodeId "pi-0") (C.outletIndex 0) (C.nodeId "osc-0") (C.inletIndex 0)
        , C.Connect (C.nodeId "num-0") (C.outletIndex 0) (C.nodeId "osc-0") (C.inletIndex 1)
        , C.Send (C.nodeId "osc-0") (C.inletIndex 0) (C.encodedValue "N 20.0")
        , C.SendO (C.nodeId "num-0") (C.outletIndex 0) (C.encodedValue "N 40.0")
        , C.Connect (C.nodeId "pi-0") (C.outletAlias "foo") (C.nodeId "osc-0") (C.inletAlias "bar")
        , C.Send (C.nodeId "osc-0") (C.inletAlias "foo") (C.encodedValue "N 20.0")
        , C.SendO (C.nodeId "num-0") (C.outletAlias "bar") (C.encodedValue "N 40.0")
        , C.Move (C.nodeId "pi-0") (C.coord 20) (C.coord 30)
        ]


spec :: Spec Unit
spec = do

  describe "Parses NDF File properly" $ do

    it "parsing works for version 0.1" $
      parses NdfFile.toNdfCode sampleNdf_0_1_Text expected_0_1_Ndf NdfFile.parser
