module Test.Spec.NdfFile where

import Prelude

import Data.Maybe (Maybe(..))

import Test.Spec (Spec, describe, it, pending')
import Test.Spec.Util.Parsing (parses)

import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile.Command (Command(..)) as C
import Noodle.Text.NdfFile.Newtypes as C
import Noodle.Text.NdfFile (from, from_, init_, toNdfCode) as NdfFile
import Noodle.Text.NdfFile.NodeDef as ND
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


sampleNdf_0_2_Text_OnlyCmds :: String
sampleNdf_0_2_Text_OnlyCmds =
    """hydra 0.1 0.2
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


sampleNdf_0_2_Text_OnlyDefs :: String
sampleNdf_0_2_Text_OnlyDefs =
    """hydra 0.1 0.2
: color : colorama :: <what:Texture {Empty} -> amount:Value {Number 0.005}> => tex:Texture {Empty}
: source : prev :: <todo:TODO {TODO}> => tex:Texture {Empty} /-| H.Empty |-/
: source : solid :: <r:Value -> g:Value -> b:Value -> a:Value {Number 1.0}> => tex:Texture {Empty} /-| H.Start $ H.Solid { r, g, b, a } |-/
: feed : number :: <in:Value {Number 0.0}> => num:Value {Number 0.0}
: test : family :: <> => <>
: synth : pi :: <> => out:Value {Pi}
: synth : mouse :: <> => <x:Value {MouseX} -> y:Value {MouseY}>
: extsource : initVideo :: <src:Source {defaultSource} -> url:String {None}> => <>
$ mouse :: /-| do
  foo
|-/
: test2 : family2 :: <> => <>"""


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


expected_0_2_Ndf_OnlyCmds :: NdfFile
expected_0_2_Ndf_OnlyCmds =
    NdfFile.from_ { toolkit : "hydra", toolkitVersion : 0.1, ndfVersion : 0.2 }
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


expected_0_2_Ndf_OnlyDefs :: NdfFile
expected_0_2_Ndf_OnlyDefs =
    NdfFile.from_ { toolkit : "hydra", toolkitVersion : 0.1, ndfVersion : 0.2 }
        [ C.DefineNode $ ND.qdef
            { group : "color", family : "colorama"
            , inputs :
              [ ND.i $ ND.chtv "what" "Texture" "Empty"
              , ND.i $ ND.chtv "amount" "Value" "Number 0.005"
              ]
            , outputs :
              [ ND.o $ ND.chtv "tex" "Texture" "Empty"
              ]
            }
        , C.DefineNode $ ND.qdefp
            { group : "source", family : "prev"
            , inputs :
              [ ND.i $ ND.chtv "todo" "TODO" "TODO"
              ]
            , outputs :
              [ ND.o $ ND.chtv "tex" "Texture" "Empty"
              ]
            , process : " H.Empty "
            }
        , C.DefineNode $ ND.qdefp
            { group : "source", family : "solid"
            , inputs :
              [ ND.i $ ND.cht "r" "Value"
              , ND.i $ ND.cht "g" "Value"
              , ND.i $ ND.cht "b" "Value"
              , ND.i $ ND.chtv "a" "Value" "Number 1.0"
              ]
            , outputs :
              [ ND.o $ ND.chtv "tex" "Texture" "Empty"
              ]
            , process : " H.Start $ H.Solid { r, g, b, a } "
            }
        , C.DefineNode $ ND.qdef
            { group : "feed", family : "number"
            , inputs :
              [ ND.i $ ND.chtv "in" "Value" "Number 0.0"
              ]
            , outputs :
              [ ND.o $ ND.chtv "num" "Value" "Number 0.0"
              ]
            }
        , C.DefineNode $ ND.qdef { group : "test", family : "family", inputs : [], outputs : [] }
        , C.DefineNode $ ND.qdef
            { group : "synth", family : "pi"
            , inputs : []
            , outputs :
              [ ND.o $ ND.chtv "out" "Value" "Pi"
              ]
            }
        , C.DefineNode $ ND.qdef
            { group : "synth", family : "mouse"
            , inputs : []
            , outputs :
              [ ND.o $ ND.chtv "x" "Value" "MouseX"
              , ND.o $ ND.chtv "y" "Value" "MouseY"
              ]
            }
        , C.DefineNode $ ND.qdef
            { group : "extsource", family : "initVideo"
            , inputs :
              [ ND.i $ ND.chtv "src" "Source" "defaultSource"
              , ND.i $ ND.chtv "url" "String" "None"
              ]
            , outputs : [ ]
            }
        , C.AssignProcess $ ND.qassign "mouse" " do\n  foo\n"
        , C.DefineNode $ ND.qdef { group : "test2", family : "family2", inputs : [], outputs : [] }
        ]


spec :: Spec Unit
spec = do

  describe "Parses NDF File properly" $ do

    it "parsing works for version 0.1" $
      parses NdfFile.toNdfCode sampleNdf_0_1_Text expected_0_1_Ndf NdfFile.parser

    pending' "parsing works for version 0.2 (empty file)" $
      parses NdfFile.toNdfCode "hydra 0.1 0.2\n" (NdfFile.init_ { toolkit : "hydra", toolkitVersion : 0.1, ndfVersion : 0.2 }) NdfFile.parser

    it "parsing works for version 0.2 (only commands)" $
      parses NdfFile.toNdfCode sampleNdf_0_2_Text_OnlyCmds expected_0_2_Ndf_OnlyCmds NdfFile.parser

    it "parsing works for version 0.2 (only definitions)" $
      parses NdfFile.toNdfCode sampleNdf_0_2_Text_OnlyDefs expected_0_2_Ndf_OnlyDefs NdfFile.parser