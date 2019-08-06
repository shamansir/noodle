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
import Rpd.API.Action.Sequence ((</>))
import Rpd.API.Action.Sequence as Actions
import Rpd.API.Action.Sequence as R

import Rpd.Render.Minimal (Renderer(..), make, once) as Render
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
      expectToRender stringRenderer compareStrings' network $ List.singleton
          $ String.trim stringSample
      expectToRenderMUV terminalRenderer compareMultiline' network $ List.singleton
          -- ML.from' "{>}"
          $ ML.empty' (100 /\ 100)
      pure unit
    it "rendering the single node works" do
      let
        singleNodeNW = Actions.init
          </> R.addPatch "foo"
          </> R.addNode (toPatch "foo") "bar" Node
      stringSample <- liftEffect $ loadSample "SingleNode.String"
      expectToRender stringRenderer compareStrings' singleNodeNW $ List.singleton
          $ String.trim stringSample
      expectToRenderMUV terminalRenderer compareMultiline' singleNodeNW $ List.singleton
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
      expectToRender stringRenderer compareStrings' severalNodesNW $ List.singleton
          $ String.trim stringSample
      expectToRenderMUV terminalRenderer compareMultiline' severalNodesNW $ List.singleton
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
      expectToRender stringRenderer compareStrings' nodeWithInletsAndOutletsNW $ List.singleton
        $ String.trim stringSample
      expectToRenderMUV
        terminalRenderer compareMultiline' nodeWithInletsAndOutletsNW $ List.singleton
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
      expectToRender stringRenderer compareStrings' withConnectionNW $ List.singleton
        $ String.trim stringSample
      expectToRenderMUV terminalRenderer compareMultiline' withConnectionNW $ List.singleton
        $ ML.empty' (100 /\ 100)
          # ML.inject (0 /\ 0) (ML.toMultiline terminalSample)
    it "rendering the erroneous network responds with the error" do
      let
        erroneousNW = Actions.init
          -- add inlet to non-exising node
          </> R.addInlet (toNode "idont" "exist") "foo" Channel
      stringSample <- liftEffect $ loadSample "Error.String"
      expectToRender stringRenderer compareStrings' erroneousNW $ List.singleton
        $ String.trim stringSample
      expectToRenderMUV terminalRenderer compareMultiline' erroneousNW $ List.singleton
        $ ML.from' "ERR: "
      pure unit

  describe "dynamic rendering" do
    describe "core renderer" do
      pending' "applies commands to the network" do
        let
          renderer = stringRendererWithOptions { showUuid : true }
          toolkit =
            R.Toolkit (R.ToolkitName "foo") $ const R.emptyNode
          emptyNW = Actions.init
        stringSample <- liftEffect $ loadSample "SingleNode.String"
        expectToRenderSeq renderer compareStrings toolkit emptyNW ""
          $ List.fromFoldable
              [ C.Bang /\ ""
              , C.AddNode (toPatch "foo") "bar" Node /\ ""
              ]
    describe "MUV renderer" do
      pending' "applies commands to the network" do
        let
          renderer = RenderMUV.fromCore $ stringRendererWithOptions { showUuid : true }
          toolkit =
            R.Toolkit (R.ToolkitName "foo") $ const R.emptyNode
          emptyNW = myRpd
        stringSample <- liftEffect $ loadSample "SingleNode.String"
        expectToRenderSeqMUV renderer compareStrings toolkit emptyNW ""
          $ List.fromFoldable
              [ C.Bang /\ ""
              , C.AddNode (toPatch "foo") "bar" Node /\ ""
              ]


    -- TODO:
    -- be able to send messages from the insides
    -- more connections
    -- node with processF
    -- shows data in the inlets/outlets
    -- toolkits and channels renderers
    -- selecting nodes
    -- status: should store commands
    -- should accept commands

  describe "possible rendering issues" $ do
    it "MUV: calls model updates only required number of times" $ do
      let
        adaptToTrack :: forall d c n. Array (C.Command d c n) -> TrackedCommands d c n
        adaptToTrack cmdArr = Right <$> List.fromFoldable cmdArr
      let toolkit =
            R.Toolkit (R.ToolkitName "foo") $ const R.emptyNode
      { event, push } <- liftEffect Event.create
      let
        { first : firstView, next : nextViews }
          = RenderMUV.make' { event, push } toolkit myRpd renderTrackUpdates
      commandsTrack <- liftEffect $ Ref.new List.Nil
      stop <- liftEffect $ Event.subscribe nextViews \v -> do
                  v >>= \commands -> Ref.modify (const commands) commandsTrack
      currentCommands <- liftEffect $ Ref.read commandsTrack
      currentCommands `shouldEqual` List.Nil
      -- FIXME: why commands are sent with `Right` here and with `Left` above?
      liftEffect $ push $ Right C.Bang
      currentCommands' <- liftEffect $ Ref.read commandsTrack
      currentCommands' `shouldEqual` (adaptToTrack [ C.Bang ])
      liftEffect $ push $ Right C.Bang
      currentCommands'' <- liftEffect $ Ref.read commandsTrack
      currentCommands'' `shouldEqual` (adaptToTrack [ C.Bang, C.Bang ])
      liftEffect $ push $ Right $ C.AddPatch "foo"
      currentCommands''' <- liftEffect $ Ref.read commandsTrack
      currentCommands''' `shouldEqual`
          (adaptToTrack [ C.AddPatch "foo", C.Bang, C.Bang ])
      liftEffect stop
    it "MUV: calls view updates only required number of times" $ do
      let toolkit =
            R.Toolkit (R.ToolkitName "foo") $ const R.emptyNode
      { event, push } <- liftEffect Event.create
      let
        { first : firstView, next : nextViews }
          = RenderMUV.make' { event, push } toolkit myRpd renderUnit
      callCount <- liftEffect $ Ref.new 0
      stop <- liftEffect $ Event.subscribe nextViews \v -> do
                  v >>= \v' -> Ref.modify ((+) 1) callCount
      currentCalls <- liftEffect $ Ref.read callCount
      currentCalls `shouldEqual` 0
      liftEffect $ push $ Right C.Bang
      currentCalls' <- liftEffect $ Ref.read callCount
      currentCalls' `shouldEqual` 1
      liftEffect $ push $ Right C.Bang
      currentCalls'' <- liftEffect $ Ref.read callCount
      currentCalls'' `shouldEqual` 2
      liftEffect $ push $ Right $ C.AddPatch "foo"
      currentCalls''' <- liftEffect $ Ref.read callCount
      currentCalls''' `shouldEqual` 3
      liftEffect stop
    it "MUV: errors are passed to the view" $ do
      let toolkit =
            R.Toolkit (R.ToolkitName "foo") $ const R.emptyNode
      { event, push } <- liftEffect Event.create
      let
        { first : firstView, next : nextViews }
          = RenderMUV.make' { event, push } toolkit myRpd renderTrackErrors
      errorsTrack <- liftEffect $ Ref.new Nothing
      stop <- liftEffect $ Event.subscribe nextViews \v -> do
                  v >>= \error -> Ref.modify (const error) errorsTrack
      currentError <- liftEffect $ Ref.read errorsTrack
      currentError `shouldEqual` Nothing
      -- FIXME: why commands are sent with `Right` here and with `Left` above?
      liftEffect $ push $ Right C.Bang
      currentError' <- liftEffect $ Ref.read errorsTrack
      currentError' `shouldEqual` Nothing
      liftEffect $ push $ Right $ C.AddPatch "foo"
      currentError'' <- liftEffect $ Ref.read errorsTrack
      currentError'' `shouldEqual` Nothing
      liftEffect $ push $ Right $ C.AddNode (toPatch "foo") "bar" Node
      currentError''' <- liftEffect $ Ref.read errorsTrack
      currentError''' `shouldEqual` Nothing
      liftEffect $ push $ Right $ C.AddNode (toPatch "bar") "bar" Node -- patch does not exists
      currentError4 <- liftEffect $ Ref.read errorsTrack
      currentError4 `shouldEqual` (Just (R.RpdError ""))
      liftEffect stop



loadSample :: String -> Effect String
loadSample name =
  readTextFile UTF8 $ "test/_samples/" <> name <> ".sample"


expectToRender
  :: forall d c n view
   . Render.Renderer d c n view
  -> CompareViewsAff view
  -> R.ActionList d c n
  -> view
  -> Aff Unit
expectToRender renderer compareViews rpd expectation = do
  result <- liftEffect $ Render.once renderer rpd
  result `compareViews` expectation


expectToRenderMUV
  :: forall d c n model view action effect
   . RenderMUV.Renderer d c n model view action effect
  -> CompareViewsAff view
  -> R.Rpd (R.Network d c n)
  -> view
  -> Aff Unit
expectToRenderMUV renderer compareViews rpd expectation = do
  result <- liftEffect $ RenderMUV.once renderer rpd
  result `compareViews` expectation


expectToRenderSeq
  :: forall d c n view
   . Render.Renderer d c n view
  -> CompareViews view
  -> R.Toolkit d c n
  -> R.ActionList d c n
  -> view
  -> List (C.Command d c n /\ view)
  -> Aff Unit
expectToRenderSeq renderer compareViews toolkit rpd firstExpectation nextExpectations = do
  { event, push } <- liftEffect Event.create
  let
    { first : firstView, next : nextViews }
        = Render.make' { event, push } toolkit rpd renderer
  firstView `compareViewsAff` firstExpectation
  failuresRef <- liftEffect $ Ref.new List.Nil
  let
    checkNextViews
        = Event.fold (foldingF push failuresRef) nextViews
            $ pure nextExpectations
  cancel <- liftEffect $ Event.subscribe checkNextViews liftEffect
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
    foldingF = foldExpectations compareViews


expectToRenderSeqMUV
  :: forall d c n model view msg
   . RenderMUV.Renderer d c n model view msg
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
  cancel <- liftEffect $ Event.subscribe checkNextViews liftEffect
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
    foldingF = foldExpectations compareViews


foldExpectations
  :: forall d c n view
   . CompareViews view
  -> (C.Command d c n -> Effect Unit)
  -> Ref (List String)
  -> Effect view
  -> Effect (List (C.Command d c n /\ view))
  -> Effect (List (C.Command d c n /\ view))
foldExpectations compareViews push failuresRef nextView chain = do
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


renderUnit :: forall d c n. RenderMUV.Renderer d c n Unit Unit Unit
renderUnit =
  RenderMUV.Renderer
    { from : unit
    , init : unit
    , update
    , view
    }
  where
    update _ ( model /\ _ ) = ( model /\ [] )
    view (RenderMUV.PushF push) v = unit


type TrackedCommands d c n = List (Either Unit (C.Command d c n))


-- renderTrackUpdates
--   :: forall d c n
--    . RenderMUV.Renderer d c n (TrackedCommands d c n) Unit Unit
-- renderTrackUpdates =
--   RenderMUV.Renderer
--     { from : unit
--     , init : List.Nil
--     , update
--     , view
--     }
--   where
--     update msgOrCmd ( model /\ _ ) = ( (msgOrCmd : model) /\ [] )
--     view (RenderMUV.PushF push) v = unit


renderTrackUpdates
  :: forall d c n
   . RenderMUV.Renderer d c n (TrackedCommands d c n) (TrackedCommands d c n) Unit
renderTrackUpdates =
  RenderMUV.Renderer
    { from : List.Nil
    , init : List.Nil
    , update
    , view
    }
  where
    update msgOrCmd ( model /\ _ ) = ( (msgOrCmd : model) /\ [] )
    view _ errOrModel =
      case errOrModel of
        Left err -> List.Nil
        Right ( model /\ _ ) -> model


renderTrackErrors
  :: forall d c n
   . RenderMUV.Renderer d c n Unit (Maybe R.RpdError) Unit
renderTrackErrors =
  RenderMUV.Renderer
    { from : Nothing
    , init : unit
    , update
    , view
    }
  where
    update msgOrCmd ( model /\ _ ) = ( model /\ [] )
    view _ errOrModel =
      case errOrModel of
        Left err -> Just err
        Right _ -> Nothing


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
