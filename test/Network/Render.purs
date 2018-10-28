module RpdTest.Network.Render
    ( spec ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff())
import Effect.Console (log)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Color (colored, Color(..))

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Rpd (init) as R
import Rpd.API as R
import Rpd.API ((</>))
import Rpd.Path
import Rpd.Network (Network) as R
import Rpd.Render (once, Renderer) as Render
import Rpd.RenderMUV (once, Renderer) as RenderMUV
import Rpd.Renderer.Terminal (terminalRenderer)
import Rpd.Renderer.Terminal.Multiline as ML
import Rpd.Renderer.String (stringRenderer)


data MyData
  = Bang
  | Value Int

type MyRpd = R.Rpd (R.Network MyData)


myRpd :: MyRpd
myRpd =
  R.init "foo"


spec :: Spec Unit
spec =
  describe "rendering" do
    it "rendering the empty network works" do
      stringSample <- liftEffect $ loadSample "Empty.String"
      expectToRenderOnce stringRenderer myRpd stringSample
      expectToRenderOnceMUV terminalRenderer myRpd $
        -- ML.from' "{>}"
        ML.empty' (100 /\ 100)
      pure unit
    it "rendering the single node works" do
      let
        singleNodeNW = myRpd
          </> R.addPatch "foo"
          </> R.addNode (patchId 0) "bar"
      stringSample <- liftEffect $ loadSample "SingleNode.String"
      expectToRenderOnce stringRenderer singleNodeNW stringSample
      expectToRenderOnceMUV terminalRenderer singleNodeNW $
        ML.empty' (100 /\ 100)
           # ML.place (0 /\ 0) "[]bar[]"
    it "rendering several nodes works" do
      let
        severalNodesNW = myRpd
          </> R.addPatch "foo0"
          </> R.addNode (patchId 0) "bar00"
          </> R.addNode (patchId 0) "bar01"
          </> R.addNode (patchId 0) "bar02"
          </> R.addPatch "foo1"
          </> R.addNode (patchId 1) "bar10"
          </> R.addNode (patchId 1) "bar11"
      stringSample <- liftEffect $ loadSample "SeveralNodes.String"
      terminalSample <- liftEffect $ loadSample "SeveralNodes.Terminal"
      expectToRenderOnce stringRenderer severalNodesNW stringSample
      expectToRenderOnceMUV terminalRenderer severalNodesNW
        $ ML.empty' (100 /\ 100)
          # ML.inject (0 /\ 0) (ML.toMultiline terminalSample)
      pure unit
    it "rendering a node with inelts and outlets works" do
      let
        nodeWithInletsAndOutletsNW = myRpd
          </> R.addPatch "foo"
          </> R.addNode (patchId 0) "bar"
          </> R.addInlet (nodePath 0 0) "buz1"
          </> R.addInlet (nodePath 0 0) "buz2"
          </> R.addOutlet (nodePath 0 0) "abc1"
          </> R.addOutlet (nodePath 0 0) "abc2"
      stringSample <- liftEffect $ loadSample "NodeWithInletsAndOutlets.String"
      terminalSample <- liftEffect $ loadSample "NodeWithInletsAndOutlets.Terminal"
      expectToRenderOnce stringRenderer nodeWithInletsAndOutletsNW stringSample
      expectToRenderOnceMUV terminalRenderer nodeWithInletsAndOutletsNW
        $ ML.empty' (100 /\ 100)
          # ML.inject (0 /\ 0) (ML.toMultiline terminalSample)
      pure unit
    it "rendering the connections works" do
      let
        withConnectionNW = myRpd
          </> R.addPatch "foo"
          </> R.addNode (patchId 0) "src"
          </> R.addOutlet (nodePath 0 0) "srco"
          </> R.addNode (patchId 0) "dst"
          </> R.addInlet (nodePath 0 1) "dsti"
          </> R.connect (outletPath 0 0 0) (inletPath 0 1 0)
      stringSample <- liftEffect $ loadSample "WithConnection.String"
      terminalSample <- liftEffect $ loadSample "WithConnection.Terminal"
      expectToRenderOnce stringRenderer withConnectionNW stringSample
      expectToRenderOnceMUV terminalRenderer withConnectionNW
        $ ML.empty' (100 /\ 100)
          # ML.inject (0 /\ 0) (ML.toMultiline terminalSample)
    it "rendering the erroneous network responds with the error" do
      let
        erroneousNW = myRpd
          -- add inlet to non-exising node
          </> R.addInlet (nodePath 0 0) "foo"
      stringSample <- liftEffect $ loadSample "Error.String"
      expectToRenderOnce stringRenderer erroneousNW stringSample
      expectToRenderOnceMUV terminalRenderer erroneousNW $ ML.from' "ERR: "
      pure unit
    -- TODO:
    -- more connections
    -- node with processF
    -- selecting nodes
    -- status: should store commands
    -- should accept commands


loadSample :: String -> Effect String
loadSample name =
  readTextFile UTF8 $ "test/Network/Samples/" <> name <> ".sample"


expectToRenderOnce
  :: forall d
   . Render.Renderer d String
  -> R.Rpd (R.Network d)
  -> String
  -> Aff Unit
expectToRenderOnce renderer rpd expectation = do
  result <- liftEffect $ Render.once renderer rpd
  (result <> "\n") `compareStrings` expectation


expectToRenderOnceMUV
  :: forall d x
   . RenderMUV.Renderer d x ML.Multiline
  -> R.Rpd (R.Network d)
  -> ML.Multiline
  -> Aff Unit
expectToRenderOnceMUV renderer rpd expectation = do
  result <- liftEffect $ RenderMUV.once renderer rpd
  result `compareViews` expectation


compareStrings :: String -> String -> Aff Unit
compareStrings s1 s2 =
  when (s1 /= s2) $
    fail $ "\n-----\n" <> s1 <> "\n\n≠\n\n" <> s2 <> "\n-----"


compareViews :: ML.Multiline -> ML.Multiline -> Aff Unit
compareViews v1 v2 =
  case v1 `ML.compare'` v2 of
    ML.Match /\ _ -> pure unit
    ML.Unknown /\ _ -> do
      fail $ "Comparison failed, reason is unknown"
    ML.DiffSize (wl /\ hl) (wr /\ hr)
      /\ Just (sampleLeft /\ sampleRight) -> do
      fail $ "Sizes are different: " <>
        show wl <> "x" <> show hl <> " (left) vs " <>
        show wr <> "x" <> show hr <> " (right)\n\n" <>
        show sampleLeft <> "\n\n" <> show sampleRight
    ML.DiffSize (wl /\ hl) (wr /\ hr)
      /\ Nothing -> do
      fail $ "Sizes are different: " <>
        show wl <> "x" <> show hl <> " (left) vs " <>
        show wr <> "x" <> show hr <> " (right)"
    ML.DiffAt (x /\ y) /\ Just (sampleLeft /\ sampleRight) -> do
      fail $ "Views are different:\n\n" <>
        show sampleLeft <> "\n\n" <> show sampleRight
    ML.DiffAt (x /\ y) /\ Nothing-> do
      fail $ "Views are different."
  -- when (v1 /= v2) $ do
  --   --liftEffect $ log $ colored Fail "aaa"
  --   fail $ show v1 <> " ≠ " <> show v2
