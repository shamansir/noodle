module RpdTest.Render
    ( spec ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.List (List, (:))
import Data.List as List
import Data.Either (Either(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Traversable (traverse, traverse_)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff())
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Aff (delay)
import Effect.Console (log)

import Test.Spec (Spec, describe, it, pending')
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Color (colored, Color(..))

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import FRP.Event as Event

-- import Rpd.API as R
import Rpd.Path
import Rpd.Network (Network) as R
import Rpd.Network (empty) as Network
import Rpd.Toolkit as R
import Rpd.Toolkit (empty) as Toolkit
import Rpd.API.Action.Sequence ((</>), ActionList(..))
import Rpd.API.Action.Sequence as Actions
import Rpd.API.Action.Sequence as R

import Rpd.Render.Minimal (Renderer(..), make, once, PushF(..)) as Render
import Rpd.Render.MUV (Renderer(..), make, once, PushF(..)) as RenderMUV
import Rpd.Renderer.Terminal (terminalRenderer)
import Rpd.Renderer.Terminal.Multiline as ML
import Rpd.Renderer.String (stringRenderer, stringRendererWithOptions)


data MyData
  = Bang
  | Value Int


data Channel = Channel


data Node = Node


toolkit :: R.Toolkit MyData Channel Node
toolkit = Toolkit.empty "foo"


network :: R.Network MyData Channel Node
network = Network.empty "foo"


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


spec :: Spec Unit
spec = do

  describe "static rendering" do

    it "rendering the empty network works" do
      stringSample <- liftEffect $ loadSample "Empty.String"
      expectToRender stringRenderer toolkit compareStrings' Actions.init
          $ String.trim stringSample
      expectToRenderMUV terminalRenderer toolkit compareMultiline' Actions.init
          -- ML.from' "{>}"
          $ ML.empty' (100 /\ 100)
      pure unit
    it "rendering the single node works" do
      let
        singleNodeNW = Actions.init
          </> R.addPatch "foo"
          </> R.addNode (toPatch "foo") "bar" Node
      stringSample <- liftEffect $ loadSample "SingleNode.String"
      expectToRender stringRenderer toolkit compareStrings' singleNodeNW
          $ String.trim stringSample
      expectToRenderMUV terminalRenderer toolkit compareMultiline' singleNodeNW
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
      expectToRender stringRenderer toolkit compareStrings' severalNodesNW
          $ String.trim stringSample
      expectToRenderMUV terminalRenderer toolkit compareMultiline' severalNodesNW
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
      expectToRender stringRenderer toolkit compareStrings' nodeWithInletsAndOutletsNW
        $ String.trim stringSample
      expectToRenderMUV
        terminalRenderer toolkit compareMultiline' nodeWithInletsAndOutletsNW
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
      expectToRender stringRenderer toolkit compareStrings' withConnectionNW
        $ String.trim stringSample
      expectToRenderMUV terminalRenderer toolkit compareMultiline' withConnectionNW
        $ ML.empty' (100 /\ 100)
          # ML.inject (0 /\ 0) (ML.toMultiline terminalSample)
    it "rendering the erroneous network responds with the error" do
      let
        erroneousNW = Actions.init
          -- add inlet to non-exising node
          </> R.addInlet (toNode "idont" "exist") "foo" Channel
      stringSample <- liftEffect $ loadSample "Error.String"
      expectToRender stringRenderer toolkit compareStrings' erroneousNW
        $ String.trim stringSample
      expectToRenderMUV terminalRenderer toolkit compareMultiline' erroneousNW
        $ ML.from' "ERR: "
      pure unit

  describe "dynamic rendering" do
    describe "core renderer" do
      pending' "applies actions to the network" do
        let
          renderer = stringRendererWithOptions { showUuid : true }
          toolkit' =
            R.Toolkit (R.ToolkitName "foo") $ const R.emptyNode
          emptyNW = Actions.init
        stringSample <- liftEffect $ loadSample "SingleNode.String"
        expectToRender renderer toolkit' compareStrings'
          (Actions.init
            -- </> R.Bang
            </> R.addNode (toPatch "foo") "bar" Node)
          stringSample
        -- expectToRenderSeq renderer compareStrings toolkit emptyNW ""
        --   $ List.fromFoldable
        --       [ C.Bang /\ ""
        --       , C.AddNode (toPatch "foo") "bar" Node /\ ""
        --       ]
    -- describe "MUV renderer" do
    --   pending' "applies commands to the network" do
    --     let
    --       renderer = RenderMUV.make (stringRendererWithOptions { showUuid : true })
    --       toolkit =
    --         R.Toolkit (R.ToolkitName "foo") $ const R.emptyNode
    --     stringSample <- liftEffect $ loadSample "SingleNode.String"
    --     expectToRender renderer compareStrings toolkit
    --       (Actions.init
    --         -- </> R.Bang
    --         </> R.addNode (toPatch "foo") "bar" Node)
    --       stringSample


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


expectToRender
  :: forall d c n view
   . Render.Renderer d c n view
  -> R.Toolkit d c n
  -> CompareViewsAff view
  -> R.ActionList d c n
  -> view
  -> Aff Unit
expectToRender renderer toolkit compareViews (ActionList actions) expectation = do
  maybeLastView <- liftEffect $ do
    lastView <- Ref.new Nothing
    { push, next : views, stop }
          <- Render.make renderer toolkit $ Network.empty "foo"
    _ <- Event.subscribe views (flip Ref.write lastView <<< Just)
    _ <- case push of
          Render.PushF pushAction ->
              traverse_ pushAction actions
    _ <- stop
    Ref.read lastView
  maybeLastView # maybe (fail "no views were recevied") (compareViews expectation)


expectToRenderMUV
  :: forall d c n model view action effect
   . RenderMUV.Renderer d c n model view action effect
  -> R.Toolkit d c n
  -> CompareViewsAff view
  -> R.ActionList d c n
  -> view
  -> Aff Unit
expectToRenderMUV renderer toolkit compareViews (ActionList actions) expectation = do
  maybeLastView <- liftEffect $ do
    lastView <- Ref.new Nothing
    { push, next : views, stop }
          <- RenderMUV.make renderer toolkit $ Network.empty "foo"
    _ <- Event.subscribe views (flip Ref.write lastView <<< Just)
    _ <- case push of
          RenderMUV.PushF pushAction ->
              traverse_ (pushAction <<< Right) actions
    _ <- stop
    Ref.read lastView
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
    ML.DiffAt (x /\ y) /\ Just (sampleLeft /\ sampleRight) -> do
      fail $ "Views are different:\n\n" <>
        show sampleLeft <> "\n\n" <> show sampleRight
    ML.DiffAt (x /\ y) /\ Nothing -> do
      fail $ "Views are different."
  -- when (v1 /= v2) $ do
  --   --liftEffect $ log $ colored Fail "aaa"
  --   fail $ show v1 <> " ≠ " <> show v2
