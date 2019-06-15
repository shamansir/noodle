module RpdTest.Render
    ( spec ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.List (List, (:))
import Data.List as List
import Data.Either (Either(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Traversable (traverse)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff())
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Aff (delay)
import Effect.Console (log)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Color (colored, Color(..))

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import FRP.Event as Event

import Rpd (init) as R
import Rpd.API as R
import Rpd.API ((</>))
import Rpd.Path
import Rpd.Network (Network) as R
import Rpd.Toolkit as R
import Rpd.Command as C

import Rpd.Render (once, Renderer) as Render
import Rpd.Render.MUV (once, Renderer, make', PushF) as RenderMUV
import Rpd.Renderer.Terminal (terminalRenderer)
import Rpd.Renderer.Terminal.Multiline as ML
import Rpd.Renderer.String (stringRenderer)


data MyData
  = Bang
  | Value Int


data Channel = Channel


data Node = Node


type MyRpd = R.Rpd (R.Network MyData Channel Node)


type CompareViews view = view -> view -> Either String Unit
type CompareViewsAff view = view -> view -> Aff Unit


myRpd :: MyRpd
myRpd =
  R.init "foo"


spec :: Spec Unit
spec =
  describe "static rendering" do
    it "rendering the empty network works" do
      stringSample <- liftEffect $ loadSample "Empty.String"
      expectToRenderOnce stringRenderer compareStrings myRpd
        $ String.trim stringSample
      expectToRenderOnceMUV terminalRenderer compareML myRpd $
        -- ML.from' "{>}"
        ML.empty' (100 /\ 100)
      pure unit
    it "rendering the single node works" do
      let
        singleNodeNW = myRpd
          </> R.addPatch "foo"
          </> R.addNode (toPatch "foo") "bar" Node
      stringSample <- liftEffect $ loadSample "SingleNode.String"
      expectToRenderOnce stringRenderer compareStrings singleNodeNW
        $ String.trim stringSample
      expectToRenderOnceMUV terminalRenderer compareML singleNodeNW $
        ML.empty' (100 /\ 100)
           # ML.place (0 /\ 0) "[]bar[]"
    it "rendering several nodes works" do
      let
        severalNodesNW = myRpd
          </> R.addPatch "foo0"
          </> R.addNode (toPatch "foo0") "bar00" Node
          </> R.addNode (toPatch "foo0") "bar01" Node
          </> R.addNode (toPatch "foo0") "bar02" Node
          </> R.addPatch "foo1"
          </> R.addNode (toPatch "foo1") "bar10" Node
          </> R.addNode (toPatch "foo1") "bar11" Node
      stringSample <- liftEffect $ loadSample "SeveralNodes.String"
      terminalSample <- liftEffect $ loadSample "SeveralNodes.Terminal"
      expectToRenderOnce stringRenderer compareStrings severalNodesNW
        $ String.trim stringSample
      expectToRenderOnceMUV terminalRenderer compareML severalNodesNW
        $ ML.empty' (100 /\ 100)
          # ML.inject (0 /\ 0) (ML.toMultiline terminalSample)
      pure unit
    it "rendering a node with inlets and outlets works" do
      let
        nodeWithInletsAndOutletsNW = myRpd
          </> R.addPatch "foo"
          </> R.addNode (toPatch "foo") "bar" Node
          </> R.addInlet (toNode "foo" "bar") "buz1" Channel
          </> R.addInlet (toNode "foo" "bar") "buz2" Channel
          </> R.addOutlet (toNode "foo" "bar") "abc1" Channel
          </> R.addOutlet (toNode "foo" "bar") "abc2" Channel
      stringSample <- liftEffect $ loadSample "NodeWithInletsAndOutlets.String"
      terminalSample <- liftEffect $ loadSample "NodeWithInletsAndOutlets.Terminal"
      expectToRenderOnce stringRenderer compareStrings nodeWithInletsAndOutletsNW
        $ String.trim stringSample
      expectToRenderOnceMUV terminalRenderer compareML nodeWithInletsAndOutletsNW
        $ ML.empty' (100 /\ 100)
          # ML.inject (0 /\ 0) (ML.toMultiline terminalSample)
      pure unit
    it "rendering the connections works" do
      let
        withConnectionNW = myRpd
          </> R.addPatch "foo"
          </> R.addNode (toPatch "foo") "src" Node
          </> R.addOutlet (toNode "foo" "src") "srco" Channel
          </> R.addNode (toPatch "foo") "dst" Node
          </> R.addInlet (toNode "foo" "dst") "dsti" Channel
          </> R.connect (toOutlet "foo" "src" "srco") (toInlet "foo" "dst" "dsti")
      stringSample <- liftEffect $ loadSample "WithConnection.String"
      terminalSample <- liftEffect $ loadSample "WithConnection.Terminal"
      expectToRenderOnce stringRenderer compareStrings withConnectionNW
        $ String.trim stringSample
      expectToRenderOnceMUV terminalRenderer compareML withConnectionNW
        $ ML.empty' (100 /\ 100)
          # ML.inject (0 /\ 0) (ML.toMultiline terminalSample)
    it "rendering the erroneous network responds with the error" do
      let
        erroneousNW = myRpd
          -- add inlet to non-exising node
          </> R.addInlet (toNode "idont" "exist") "foo" Channel
      stringSample <- liftEffect $ loadSample "Error.String"
      expectToRenderOnce stringRenderer compareStrings erroneousNW
        $ String.trim stringSample
      expectToRenderOnceMUV terminalRenderer compareML erroneousNW
        $ ML.from' "ERR: "
      pure unit
    describe "dynamic rendering" do
      pure unit
    -- TODO:
    -- be able to send messages from the insides
    -- more connections
    -- node with processF
    -- shows data in the inlets/outlets
    -- toolkits and channels renderers
    -- selecting nodes
    -- status: should store commands
    -- should accept commands



loadSample :: String -> Effect String
loadSample name =
  readTextFile UTF8 $ "test/_samples/" <> name <> ".sample"


expectToRenderOnce
  :: forall d c n view
   . Render.Renderer d c n view
  -> CompareViewsAff view
  -> R.Rpd (R.Network d c n)
  -> view
  -> Aff Unit
expectToRenderOnce renderer compareViews rpd expectation = do
  result <- liftEffect $ Render.once renderer rpd
  result `compareViews` expectation


expectToRenderOnceMUV
  :: forall d c n model view msg
   . RenderMUV.Renderer d c n model view msg
  -> CompareViewsAff view
  -> R.Rpd (R.Network d c n)
  -> view
  -> Aff Unit
expectToRenderOnceMUV renderer compareViews rpd expectation = do
  result <- liftEffect $ RenderMUV.once renderer rpd
  result `compareViews` expectation


data NoMsg = NoMsg


expectToRenderSeqMUV
  :: forall d c n model view
   . RenderMUV.Renderer d c n model view NoMsg
  -> CompareViews view
  -> R.Toolkit d c n
  -> R.Rpd (R.Network d c n)
  -> view
  -> List (C.Command d c n /\ view)
  -> Aff Unit
expectToRenderSeqMUV renderer compareViews toolkit rpd firstExpectation nextExpectations = do
  { event, push } <- liftEffect Event.create
  let
    { first : firstView, next : nextViews }
        = RenderMUV.make' { event, push } toolkit rpd renderer
  firstView `compareViewsAff` firstExpectation
  failuresRef <- liftEffect $ Ref.new List.Nil
  let
    checkNextViews
        = Event.fold (foldingF (push <<< Right) failuresRef) nextViews
            $ pure nextExpectations
  -- TODO: Write failures to the Ref and then read it after the timeout
  cancel <- liftEffect $ Event.subscribe checkNextViews liftEffect
  -- _ <- traverse liftEffect checkNextViews
  delay (Milliseconds 100.0)
  failures <- liftEffect $ Ref.read failuresRef
  failures `shouldEqual` List.Nil
  _ <- liftEffect cancel
  pure unit
  where
    compareViewsAff :: CompareViewsAff view
    compareViewsAff = toAffCompare compareViews
    foldingF
      :: (C.Command d c n -> Effect Unit)
      -> Ref (List String)
      -> Effect view
      -> Effect (List (C.Command d c n /\ view))
      -> Effect (List (C.Command d c n /\ view))
    foldingF push failuresRef nextView chain = do
          v <- nextView
          nextExpectations' <- chain
          let expectationsLeft = fromMaybe List.Nil $ List.tail nextExpectations'
          case List.head nextExpectations' of
            Just (msg /\ nextExpectation) -> do
              push msg
              (case nextExpectation `compareViews` v of
                  Left failure -> do
                    _ <- Ref.modify ((:) failure) failuresRef
                    pure expectationsLeft
                  Right _ ->
                    pure expectationsLeft
              )
            Nothing ->
              pure expectationsLeft

toAffCompare :: forall v. CompareViews v -> CompareViewsAff v
toAffCompare compareViews =
  \vL vR ->
    case vL `compareViews` vR of
      Left failure -> fail failure
      _ -> pure unit


compareStrings :: CompareViewsAff String
compareStrings s1 s2 =
  when (s1 /= s2) $
    fail $ "\n-----\n" <> s1 <> "\n\n≠\n\n" <> s2 <> "\n-----"


compareML :: CompareViewsAff ML.Multiline
compareML v1 v2 =
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
    ML.DiffAt (x /\ y) /\ Nothing -> do
      fail $ "Views are different."
  -- when (v1 /= v2) $ do
  --   --liftEffect $ log $ colored Fail "aaa"
  --   fail $ show v1 <> " ≠ " <> show v2
