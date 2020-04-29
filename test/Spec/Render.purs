module Rpd.Test.Spec.Render
    ( spec ) where

import Prelude

import Debug.Trace (traceM) as DT

import Data.Time.Duration (Milliseconds(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.List (List, (:))
import Data.List as List
import Data.Either (Either(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Traversable (traverse, traverse_)
import Data.Covered (carry)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff())
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Aff (delay)
import Effect.Console (log)

import Test.Spec (Spec, describe, it, pending', pending, itOnly)
import Test.Spec.Assertions (shouldEqual, fail)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import FRP.Event as Event

import FSM (doNothing)

-- import Rpd.API as R
import Rpd.Path
import Rpd.Network (Network) as R
import Rpd.Network (empty) as Network
import Rpd.Toolkit as R
import Rpd.Toolkit (empty) as Toolkit
import Rpd.API.Action.Sequence ((</>), ActionList)
import Rpd.API.Action.Sequence as Actions
import Rpd.API.Action.Sequence as R

import UI (make, once, run) as UI
import Rpd.Render.Renderer (Routed(..))
import Rpd.Render.Renderer (Renderer, Minimal) as Render
import Rpd.Render.Terminal (make, init) as TerminalRenderer
import Rpd.Render.Terminal (TerminalRenderer)
import Rpd.Render.Terminal.Multiline as ML
import Rpd.Render.String (make, makeWithOptions) as StringRenderer
import Rpd.Render.String (StringRenderer)

import Rpd.Test.Util.Spy as Spy


data MyData
  = Bang
  | Value Int


data Channel = Channel


data Node = Node


toolkit :: R.Toolkit MyData Channel Node
toolkit = Toolkit.empty "foo"


network :: R.Network MyData Channel Node
network = Network.empty "network"


derive instance eqChannel ∷ Eq Channel
derive instance eqNode ∷ Eq Node


instance showMyData :: Show MyData where
  show Bang = "Bang"
  show (Value n) = "Value " <> show n


instance showChannel :: Show Channel where
  show Channel = "Channel"


instance showNode :: Show Node where
  show Node = "Node"


type CompareViews view = view -> view -> Either String Unit
type CompareViewsAff view = view -> view -> Aff Unit


stringRenderer :: StringRenderer MyData Channel Node
stringRenderer = StringRenderer.make toolkit


terminalRenderer :: TerminalRenderer MyData Channel Node
terminalRenderer = TerminalRenderer.make toolkit


spec :: Spec Unit
spec = do

  describe "static rendering" do

    it "rendering the empty network works" do
      stringSample <- liftEffect $ loadSample "Empty.String"
      let doNothing' = Actions.init </> (R.do_ $ const $ pure unit)
      expectToRenderM stringRenderer compareStrings' doNothing'
          $ String.trim stringSample
      expectToRender terminalRenderer compareMultiline' doNothing' TerminalRenderer.init
          -- ML.from' "{>}"
          $ ML.empty' (100 /\ 100)
      pure unit

    it "rendering the single node works" do
      let
        singleNodeNW = Actions.init
          </> R.addPatch "foo"
          </> R.addNode (toPatch "foo") "bar" Node
      stringSample <- liftEffect $ loadSample "SingleNode.String"
      expectToRenderM stringRenderer compareStrings' singleNodeNW
          $ String.trim stringSample
      expectToRender terminalRenderer compareMultiline' singleNodeNW TerminalRenderer.init
          $ ML.empty' (100 /\ 100) # ML.place (0 /\ 0) "[]bar[]"

    it "rendering several nodes works" do
      let
        severalNodesNW = Actions.init
          </> R.addPatch "foo0"
          </> R.addNode (toPatch "foo0") "bar00" Node
          </> R.addNode (toPatch "foo0") "bar01" Node
          </> R.addNode (toPatch "foo0") "bar02" Node
          </> R.addPatch "foo1"
          </> R.addNode (toPatch "foo1") "bar10" Node
          </> R.addNode (toPatch "foo1") "bar11" Node
      stringSample <- liftEffect $ loadSample "SeveralNodes.String"
      terminalSample <- liftEffect $ loadSample "SeveralNodes.Terminal"
      expectToRenderM stringRenderer compareStrings' severalNodesNW
          $ String.trim stringSample
      expectToRender terminalRenderer compareMultiline' severalNodesNW TerminalRenderer.init
          $ ML.empty' (100 /\ 100)
              # ML.inject (0 /\ 0) (ML.toMultiline terminalSample)
      pure unit

    it "rendering a node with inlets and outlets works" do
      let
        nodeWithInletsAndOutletsNW = Actions.init
          </> R.addPatch "foo"
          </> R.addNode (toPatch "foo") "bar" Node
          </> R.addInlet (toNode "foo" "bar") "buz1" Channel
          </> R.addInlet (toNode "foo" "bar") "buz2" Channel
          </> R.addOutlet (toNode "foo" "bar") "abc1" Channel
          </> R.addOutlet (toNode "foo" "bar") "abc2" Channel
      stringSample <- liftEffect $ loadSample "NodeWithInletsAndOutlets.String"
      terminalSample <- liftEffect $ loadSample "NodeWithInletsAndOutlets.Terminal"
      expectToRenderM stringRenderer compareStrings' nodeWithInletsAndOutletsNW
        $ String.trim stringSample
      expectToRender
        terminalRenderer compareMultiline' nodeWithInletsAndOutletsNW TerminalRenderer.init
          $ ML.empty' (100 /\ 100)
            # ML.inject (0 /\ 0) (ML.toMultiline terminalSample)
      pure unit

    it "rendering the connections works" do
      let
        withConnectionNW = Actions.init
          </> R.addPatch "foo"
          </> R.addNode (toPatch "foo") "src" Node
          </> R.addOutlet (toNode "foo" "src") "srco" Channel
          </> R.addNode (toPatch "foo") "dst" Node
          </> R.addInlet (toNode "foo" "dst") "dsti" Channel
          </> R.connect (toOutlet "foo" "src" "srco") (toInlet "foo" "dst" "dsti")
      stringSample <- liftEffect $ loadSample "WithConnection.String"
      terminalSample <- liftEffect $ loadSample "WithConnection.Terminal"
      expectToRenderM stringRenderer compareStrings' withConnectionNW
        $ String.trim stringSample
      expectToRender terminalRenderer compareMultiline' withConnectionNW TerminalRenderer.init
        $ ML.empty' (100 /\ 100)
          # ML.inject (0 /\ 0) (ML.toMultiline terminalSample)

    it "rendering the erroneous network responds with the error" do
      let
        erroneousNW = Actions.init
          -- add inlet to a non-exising node
          </> R.addInlet (toNode "idont" "exist") "foo" Channel
      stringSample <- liftEffect $ loadSample "Error.String"
      terminalSample <- liftEffect $ loadSample "Error.Terminal"
      expectToRenderM stringRenderer compareStrings' erroneousNW
        $ String.trim stringSample
      expectToRender terminalRenderer compareMultiline' erroneousNW TerminalRenderer.init
        $ ML.empty' (100 /\ 100)
          # ML.inject (0 /\ 0) (ML.toMultiline terminalSample)

  describe "dynamic rendering" do

    describe "core renderer" do

      pending "applies actions to the network"  {- do
        let
          renderer = StringRenderer.makeWithOptions toolkit { showUuid : true }
          toolkit' =
            R.Toolkit (R.ToolkitName "foo") $ const R.emptyNode
          emptyNW = Actions.init
        stringSample <- liftEffect $ loadSample "SingleNode.String"
        expectToRenderM renderer toolkit' compareStrings'
          (Actions.init
            -- </> R.Bang
            </> R.addNode (toPatch "foo") "bar" Node)
          stringSample -}

    describe "MUV renderer" do

      pending "applies actions to the network" {- do
        let
          renderer = StringRenderer.makeWithOptions { showUuid : true }
          toolkit' =
            R.Toolkit (R.ToolkitName "foo") $ const R.emptyNode
        stringSample <- liftEffect $ loadSample "SingleNode.String"
        expectToRender renderer toolkit' compareStrings'
          (Actions.init
            -- </> R.Bang
            </> R.addNode (toPatch "foo") "bar" Node)
          stringSample -}


    -- TODO:
    -- be able to send messages from the insides
    -- more connections
    -- node with processF
    -- shows data in the inlets/outlets
    -- toolkits and channels renderers
    -- selecting nodes
    -- status: should store actions
    -- should accept actions

  describe "possible rendering issues" $ do
    pending' "MUV: errors are passed to the view" $ do
      pure unit



loadSample :: String -> Effect String
loadSample name =
  readTextFile UTF8 $ "test/_samples/" <> name <> ".sample"


expectToRenderM
  :: forall d c n view
   . Render.Minimal d c n view
  -> CompareViewsAff view
  -> R.ActionList d c n
  -> view
  -> Aff Unit
expectToRenderM renderer compareViews actions expectation = do
  maybeLastView <- liftEffect $ do
    lastViewSpy <- Spy.last
    { push, next : views }
          <- UI.run renderer $ carry $ Network.empty "network"
    _ <- Event.subscribe views $ Spy.with lastViewSpy
    _ <- traverse_ push actions
    Spy.get lastViewSpy
  maybeLastView # maybe (fail "no views were recevied") (compareViews expectation)


expectToRenderMFirst
  :: forall d c n view
   . Render.Minimal d c n view
  -> CompareViewsAff view
  -> R.ActionList d c n
  -> view
  -> Aff Unit
expectToRenderMFirst renderer compareViews actions expectation = do
  maybeFirstView <- liftEffect $ do
    firstViewSpy <- Spy.first -- the only difference with `expectToRenderM`
    { push, next : views }
          <- UI.run renderer $ carry $ Network.empty "network"
    _ <- Event.subscribe views $ Spy.with firstViewSpy
    _ <- traverse_ push actions
    Spy.get firstViewSpy
  maybeFirstView # maybe (fail "no views were recevied") (compareViews expectation)


expectToRender
  :: forall d c n action model view
   . Render.Renderer d c n action model view
  -> CompareViewsAff view
  -> R.ActionList d c n
  -> model
  -> view
  -> Aff Unit
expectToRender renderer compareViews actions model expectation = do
  maybeLastView <- liftEffect $ do
    lastViewSpy <- Spy.last
    { push, next : views }
          <- UI.run renderer $ carry $ model /\ Network.empty "network"
--    _ <- Event.subscribe views $ DT.traceM
    _ <- Event.subscribe views $ Spy.with lastViewSpy
    _ <- traverse_ (push <<< FromCore) actions
    Spy.get lastViewSpy
  maybeLastView # maybe (fail "no views were recevied") (compareViews expectation)


-- renderUnit :: forall d c n. RenderMUV.Renderer d c n Unit Unit Unit Unit
-- renderUnit =
--   RenderMUV.Renderer
--     { from : unit
--     , init : unit
--     , update
--     , view
--     }
--   where
--     update _ ( model /\ _ ) = ( model /\ [] )
--     view (RenderMUV.PushF push) v = unit


toAffCompare :: forall v. CompareViews v -> CompareViewsAff v
toAffCompare compareViews =
  \vL vR ->
    case vL `compareViews` vR of
      Left failure -> fail failure
      _ -> pure unit


compareStrings' :: CompareViewsAff String
-- compareStrings' s1 s2 =
compareStrings' =
  toAffCompare compareStrings
  -- when (s1 /= s2) $
  --   fail $ "\n-----\n" <> s1 <> "\n\n≠\n\n" <> s2 <> "\n-----"


compareStrings :: CompareViews String
compareStrings s1 s2 =
  if (s1 /= s2) then
    Left $ "\n-----\n" <> s1 <> "\n\n≠\n\n" <> s2 <> "\n-----"
  else
    Right unit


showCpsInequality :: String.CodePoint /\ String.CodePoint -> String
showCpsInequality (lcp /\ rcp) =
  "<" <> show lcp <> "> /= <" <> show rcp <> ">"


compareMultiline' :: CompareViewsAff ML.Multiline
compareMultiline' v1 v2 =
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
    ML.DiffAt (x /\ y) (lcp /\ rcp) /\ Just (sampleLeft /\ sampleRight) -> do
      fail $ "Views are different:\n\n" <>
        show sampleLeft <> "\n\n" <> show sampleRight
          <> "\n\n" <> showCpsInequality (lcp /\ rcp)
    ML.DiffAt (x /\ y) (lcp /\ rcp) /\ Nothing -> do
      fail $ "Views are different. " <> showCpsInequality (lcp /\ rcp)
  -- when (v1 /= v2) $ do
  --   --liftEffect $ log $ colored Fail "aaa"
  --   fail $ show v1 <> " ≠ " <> show v2
