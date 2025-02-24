module Test.Spec.NdfFile where

import Prelude

import Effect.Class (liftEffect)

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.UniqueHash as UH

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Test.Spec (Spec, describe, it, pending', describeOnly)
import Test.Spec.Util.Parsing (parses)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Util.Assertions (shouldEqual) as U

import Parsing (runParser) as P

import Noodle.Id (FamilyR, unsafeFamilyR) as Id
import Noodle.Raw.Id (nodeR, familyR) as Id
import Noodle.Text.NdfFile (NdfFile, (&->), (<-&))
import Noodle.Text.NdfFile.Command (Command(..)) as C
import Noodle.Text.NdfFile.Command.Op (CommandOp(..)) as C
import Noodle.Text.NdfFile.Types (coord, encodedValue, inletAlias, inletIndex, nodeInstanceId, outletAlias, outletIndex) as C
import Noodle.Text.NdfFile (from_, init, init_, toNdfCode, optimize) as NdfFile
import Noodle.Text.NdfFile.FamilyDef as ND
import Noodle.Text.NdfFile.FamilyDef.ProcessCode (ProcessCode(..)) as ND
import Noodle.Text.NdfFile.Parser (parser) as NdfFile
import Noodle.Text.NdfFile.Command.Quick as Q


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
>< num-0 0 osc-0 1
>< pi-0 foo osc-0 bar
>< pi-0 0 osc-0 0
x pi-0
x osc-0
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
$ mouse :: %┤ do
  # foo % test # {-} %% $
├%
: stated : stated :: [Unit] <in:Value {Number 0.0}> => num:Value {Number 1.0}
: stated : stated2 :: [Unit {unit}] <> => <>
: test2 : family2 :: <> => <>
@ stated : A node with some state
@ stated2 : A node with some state, version 2
"""


familyR :: String -> Id.FamilyR
familyR = Id.unsafeFamilyR


expected_0_1_Ndf :: NdfFile
expected_0_1_Ndf =
    NdfFile.from_ { toolkit : "hydra", toolkitVersion : 0.1, ndfVersion : 0.1 }
        $ C.Command Nothing <$>
        [ C.Comment "test example"
        , C.MakeNode (familyR "osc") (C.coord 40) (C.coord 60) (C.nodeInstanceId "osc-0")
        , C.MakeNode (familyR "osc") (C.coord 40) (C.coord 60) (C.nodeInstanceId "osc-1")
        , C.MakeNode (familyR "pi") (C.coord 20) (C.coord 20) (C.nodeInstanceId "pi-0")
        , C.MakeNode (familyR "number") (C.coord 40) (C.coord 40) (C.nodeInstanceId "num-0")
        , C.Comment "connect pi to osc"
        , C.Connect (C.nodeInstanceId "pi-0") (C.outletIndex 0) (C.nodeInstanceId "osc-0") (C.inletIndex 0)
        , C.Connect (C.nodeInstanceId "num-0") (C.outletIndex 0) (C.nodeInstanceId "osc-0") (C.inletIndex 1)
        , C.Send (C.nodeInstanceId "osc-0") (C.inletIndex 0) (C.encodedValue "N 20.0")
        , C.SendO (C.nodeInstanceId "num-0") (C.outletIndex 0) (C.encodedValue "N 40.0")
        , C.Connect (C.nodeInstanceId "pi-0") (C.outletAlias "foo") (C.nodeInstanceId "osc-0") (C.inletAlias "bar")
        , C.Send (C.nodeInstanceId "osc-0") (C.inletAlias "foo") (C.encodedValue "N 20.0")
        , C.SendO (C.nodeInstanceId "num-0") (C.outletAlias "bar") (C.encodedValue "N 40.0")
        , C.Move (C.nodeInstanceId "pi-0") (C.coord 20) (C.coord 30)
        ]


expected_0_2_Ndf_OnlyCmds :: NdfFile
expected_0_2_Ndf_OnlyCmds =
    NdfFile.from_ { toolkit : "hydra", toolkitVersion : 0.1, ndfVersion : 0.2 }
        $ C.Command Nothing <$>
        [ C.Comment "test example"
        , C.MakeNode (familyR "osc") (C.coord 40) (C.coord 60) (C.nodeInstanceId "osc-0")
        , C.MakeNode (familyR "osc") (C.coord 40) (C.coord 60) (C.nodeInstanceId "osc-1")
        , C.MakeNode (familyR "pi") (C.coord 20) (C.coord 20) (C.nodeInstanceId "pi-0")
        , C.MakeNode (familyR "number") (C.coord 40) (C.coord 40) (C.nodeInstanceId "num-0")
        , C.Comment "connect pi to osc"
        , C.Connect (C.nodeInstanceId "pi-0") (C.outletIndex 0) (C.nodeInstanceId "osc-0") (C.inletIndex 0)
        , C.Connect (C.nodeInstanceId "num-0") (C.outletIndex 0) (C.nodeInstanceId "osc-0") (C.inletIndex 1)
        , C.Send (C.nodeInstanceId "osc-0") (C.inletIndex 0) (C.encodedValue "N 20.0")
        , C.SendO (C.nodeInstanceId "num-0") (C.outletIndex 0) (C.encodedValue "N 40.0")
        , C.Connect (C.nodeInstanceId "pi-0") (C.outletAlias "foo") (C.nodeInstanceId "osc-0") (C.inletAlias "bar")
        , C.Send (C.nodeInstanceId "osc-0") (C.inletAlias "foo") (C.encodedValue "N 20.0")
        , C.SendO (C.nodeInstanceId "num-0") (C.outletAlias "bar") (C.encodedValue "N 40.0")
        , C.Move (C.nodeInstanceId "pi-0") (C.coord 20) (C.coord 30)
        , C.Disconnect (C.nodeInstanceId "num-0") (C.outletIndex 0) (C.nodeInstanceId "osc-0") (C.inletIndex 1)
        , C.Disconnect (C.nodeInstanceId "pi-0") (C.outletAlias "foo") (C.nodeInstanceId "osc-0") (C.inletAlias "bar")
        , C.Disconnect (C.nodeInstanceId "pi-0") (C.outletIndex 0) (C.nodeInstanceId "osc-0") (C.inletIndex 0)
        , C.RemoveNode (C.nodeInstanceId "pi-0")
        , C.RemoveNode (C.nodeInstanceId "osc-0")
        ]


expected_0_2_Ndf_OnlyDefs :: NdfFile
expected_0_2_Ndf_OnlyDefs =
    NdfFile.from_ { toolkit : "hydra", toolkitVersion : 0.1, ndfVersion : 0.2 }
        $ C.Command Nothing <$>
        [ C.DefineFamily $ ND.qdef
            { group : "color", family : "colorama"
            , inputs :
              [ ND.i $ ND.chtv "what" "Texture" "Empty"
              , ND.i $ ND.chtv "amount" "Value" "Number 0.005"
              ]
            , outputs :
              [ ND.o $ ND.chtv "tex" "Texture" "Empty"
              ]
            }
        , C.DefineFamily $ ND.qdefp
            { group : "source", family : "prev"
            , inputs :
              [ ND.i $ ND.chtv "todo" "TODO" "TODO"
              ]
            , outputs :
              [ ND.o $ ND.chtv "tex" "Texture" "Empty"
              ]
            , process : ND.Auto " H.Empty "
            }
        , C.DefineFamily $ ND.qdefp
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
            , process : ND.Auto " H.Start $ H.Solid { r, g, b, a } "
            }
        , C.DefineFamily $ ND.qdef
            { group : "feed", family : "number"
            , inputs :
              [ ND.i $ ND.chtv "in" "Value" "Number 0.0"
              ]
            , outputs :
              [ ND.o $ ND.chtv "num" "Value" "Number 0.0"
              ]
            }
        , C.DefineFamily $ ND.qdef { group : "test", family : "family", inputs : [], outputs : [] }
        , C.DefineFamily $ ND.qdef
            { group : "synth", family : "pi"
            , inputs : []
            , outputs :
              [ ND.o $ ND.chtv "out" "Value" "Pi"
              ]
            }
        , C.DefineFamily $ ND.qdef
            { group : "synth", family : "mouse"
            , inputs : []
            , outputs :
              [ ND.o $ ND.chtv "x" "Value" "MouseX"
              , ND.o $ ND.chtv "y" "Value" "MouseY"
              ]
            }
        , C.DefineFamily $ ND.qdef
            { group : "extsource", family : "initVideo"
            , inputs :
              [ ND.i $ ND.chtv "src" "Source" "defaultSource"
              , ND.i $ ND.chtv "url" "String" "None"
              ]
            , outputs : []
            }
        , C.AssignProcess $ ND.qassign "mouse" $ ND.Raw " do\n  # foo % test # {-} %% $\n"
        , C.DefineFamily $ ND.qdefs
            { group : "stated", family : "stated"
            , inputs :
              [ ND.i $ ND.chtv "in" "Value" "Number 0.0"
              ]
            , outputs :
              [ ND.o $ ND.chtv "num" "Value" "Number 1.0"
              ]
            , state : ND.stt "Unit"
            }
        , C.DefineFamily $ ND.qdefs
            { group : "stated", family : "stated2"
            , inputs : []
            , outputs : []
            , state : ND.st "Unit" "unit"
            }
        {- }, C.DefineFamily $ ND.qdefs
            { group : "stated", family : "stated3"
            , inputs : []
            , outputs : []
            , state : ND.stv "unit"
            } -}
        , C.DefineFamily $ ND.qdef { group : "test2", family : "family2", inputs : [], outputs : [] }
        , C.Documentation (familyR "stated") "A node with some state"
        , C.Documentation (familyR "stated2") "A node with some state, version 2"
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

    it "parsing works for version 0.2 (hydra toolkit v0.3)" $ do
      hydraToolkitText <- liftEffect $ readTextFile UTF8 "./ndf/hydra.v0.3.ndf"
      let eParsedCode = NdfFile.toNdfCode <$> P.runParser hydraToolkitText NdfFile.parser
      case eParsedCode of
        Right parsedCode ->
          -- We test that parsing, creating the NDF structure from file, and encoding it back,
          -- produces the same output text as it was originally in the source file
          parsedCode `U.shouldEqual` hydraToolkitText
          -- liftEffect $ writeTextFile UTF8 "./src/Hydra/hydra.v0.2.ndf" result
        Left error ->
          fail $ "failed to parse hydra.v0.3.ndf: " <> show error

  describe "optimization" $ do

      it "keeps the commands that does not need to be optimized" $ do
        node1hash <- liftEffect $ UH.generate
        node2hash <- liftEffect $ UH.generate
        let node1R = Id.nodeR (Id.familyR "family-1") node1hash
        let node2R = Id.nodeR (Id.familyR "family-2") node2hash
        let srcNdfFile =
              NdfFile.init "test" 1.0
                &-> Q.makeNode node1R { left : 10, top : 5 }
                &-> Q.makeNode node2R { left : 12, top : 5 }
            trgNdfFile =
              NdfFile.optimize srcNdfFile
        trgNdfFile `shouldEqual` srcNdfFile

      it "merges repetetive move commands" $ do
        node1hash <- liftEffect $ UH.generate
        node2hash <- liftEffect $ UH.generate
        let node1R = Id.nodeR (Id.familyR "family-1") node1hash
        let node2R = Id.nodeR (Id.familyR "family-2") node2hash
        let srcNdfFile =
              NdfFile.init "test" 1.0
                &-> Q.makeNode node1R { left : 10, top : 5 }
                &-> Q.moveNode node1R { left : 20, top : 7 }
                &-> Q.moveNode node1R { left : 20, top : 17 }
                &-> Q.makeNode node2R { left : 12, top : 5 }
                &-> Q.moveNode node2R { left : 6, top : 11 }
                &-> Q.moveNode node2R { left : 7, top : 15 }
            trgNdfFile =
              NdfFile.optimize srcNdfFile
            expNdFile =
              NdfFile.init "test" 1.0
                &-> Q.makeNode node1R { left : 10, top : 5 }
                &-> Q.moveNode node1R { left : 20, top : 17 }
                &-> Q.makeNode node2R { left : 12, top : 5 }
                &-> Q.moveNode node2R { left : 7, top : 15 }
        trgNdfFile `shouldEqual` expNdFile