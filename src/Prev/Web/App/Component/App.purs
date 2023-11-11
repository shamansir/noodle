module Prev.Web.App.Component.App where

import Prelude

import Prev.Web.Emitters  as Emitters
import Layout.Flex as Flex
import Prev.Web.LayoutRenderer (render) as Layout
import Layouts.App (layout, AppLayoutPart(..)) as App
import Layouts.PatchTabs (layout) as PatchTabs
import Layouts.PatchTabs (TabOrAdd(..)) as PT
import Web.App.Style (Style, NodeFlow)
import Web.App.Style.ClassNames as CS
import Prev.Web.Svg.Extra as HSA
import Prev.Web.App.Toolkit.UI as ToolkitUI
import CSS as CSS
import Color (rgb, rgba) as C
import Color.Extra as C
import Data.Array ((:))
import Data.Array (length, fromFoldable, mapWithIndex) as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, singleton) as NE
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Data.Vec2 ((<+>), Size)
import Data.Vec2 as V2
import Debug (spy) as Debug
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.None as HS
import Noodle.Network (Network) as Noodle
import Noodle.Network as Network
import Prev.Noodle.Node (Family, dimensions, family) as Node
import Noodle.Patch as Patch
import Noodle.Toolkit (Toolkit) as Noodle
import Noodle.Toolkit (name, nodeFamilies, nodeFamiliesCount, state, spawn) as Toolkit
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Window as Window


{- type Slots patch_action patch_state =
    ( patch :: PatchC.Slot patch_action Unit
    , tkPatch :: ToolkitUI.PatchSlot' patch_action patch_state Unit
    ) -}


font =
     { size : 7.0, family : [ "Trispace", "PT Mono", "Andale Mono", "Fira mono", "Menlo" ] }


type Slots =
    ( )


_patch = Proxy :: Proxy "patch"

_tkPatch = Proxy :: Proxy "tkPatch"

_fltc = Proxy :: Proxy "fltc"


type Input patch_state node_state d =
    { network :: Noodle.Network d
    , toolkit :: Noodle.Toolkit patch_state d
    , currentPatch :: Maybe Patch.Id
    , markings :: ToolkitUI.Markings
    , getFlags :: ToolkitUI.GetFlags
    , patchState :: patch_state
    }


type State patch_state node_state d =
    { network :: Noodle.Network d
    , toolkit :: Noodle.Toolkit patch_state d
    , currentPatch :: Maybe Patch.Id
    , markings :: ToolkitUI.Markings
    , getFlags :: ToolkitUI.GetFlags
    -- ^ same as Input
    , windowSize :: Size
    , currentFrame :: Number
    , patchState :: patch_state
    }


data Action
    = Initialize
    | SelectPatch Patch.Id
    | AddPatch
    | AddNode Node.Family
    | AnimationFrame H.SubscriptionId Number
    | WindowResize H.SubscriptionId { w :: Int, h :: Int }
    {-
    | ToPatch patch_action
    | FromPatch (ToolkitUI.InformApp patch_state)
    -}
    | Skip
    -- | HandlePatch (PatchC.Action d)


initialState
    :: forall patch_state node_state d
     . Input patch_state node_state d
    -> State patch_state node_state d
initialState { network, toolkit, currentPatch, markings, getFlags, patchState } =
    { network, toolkit, markings, getFlags, patchState
    , currentPatch
    , windowSize : 1000.0 <+> 1000.0
    , currentFrame : 0.0
    }


render
    :: forall patch_action patch_state node_state d
     . State patch_state node_state d
    -> H.ComponentHTML Action Slots Aff -- FIXME: there is MonadAff here!
render (s@{ network, toolkit, windowSize }) =
    HH.div
        [ CSS.style $ do
            CSS.fontFamily font.family $ NE.singleton CSS.sansSerif
            CSS.fontSize $ CSS.pt font.size
            -- CSS.bas
        ]
        [ HS.svg
            [ HSA.width $ V2.w windowSize
            , HSA.height $ V2.h windowSize
            ]
            $ Layout.render renderPart
            $ Flex.fitLayers windowSize App.layout
            {- ( toolkitInfo toolkit
            : addPatch
            : (patchTab <$> Network.patches network)
            ) -}
        ]
    where
        renderPart App.PatchTabs pos size =
            HS.g
                [ HSA.translateTo' pos ]
                $ Layout.render patchTab
                $ PatchTabs.layout (V2.w size) $ Tuple.fst <$> Network.patches network
            {-
            HS.g
                [ HSA.translateTo' pos ]
                ( addPatch : (patchTab <$> Network.patches network) )
            -}
        renderPart App.Body pos size =
            HS.g
                [ HSA.translateTo' pos ]
                [ toolkitInfo toolkit
                , case s.currentPatch >>= flip Network.patch network of
                    Just patch -> patchBody patch
                    Nothing -> HS.g [] []
                ]
        renderPart App.PatchBackground pos size =
            HS.rect
                [ HSA.x $ V2.x pos, HSA.y $ V2.y pos
                , HSA.width $ V2.w size, HSA.height $ V2.h size
                , HSA.fill $ Just $ C.toSvg $ C.rgba 0 0 0 0.0
                , HSA.stroke $ Just $ C.toSvg $ C.rgba 0 0 0 1.0
                , HSA.strokeWidth 1.0
                ]
        renderPart App.NodeList _ _ =
            HS.g [] []
        renderPart App.Space _ _ =
            HS.g [] []

        patchBody patch =
            HS.g [] $ (map Patch.unwrapNode >>> renderNode) <$> Patch.nodes patch
        renderNode (id /\ node) =
            let
                inletsCount /\ outletsCount = Node.dimensions node
            in
            HS.text
                [ HSA.translateTo' $ 0.0 <+> (70.0 + font.size * 2.0)
                , HSA.fill $ Just $ C.toSvg $ C.rgba 0 0 0 1.0
                ]
                [ HH.text $ id <> "::" <> Node.family node <> " :: " <> show inletsCount <> "x" <> show outletsCount
                ]
        patchTab PT.Add pos size =
            HS.text
                [ HSA.translateTo' pos
                , HSA.fill $ Just $ C.toSvg $ C.rgba 0 0 0 1.0
                , HSA.dominant_baseline $ HSA.Hanging
                , HE.onClick $ const AddPatch
                ]
                [ HH.text $ "Add Patch"
                ]
        patchTab (PT.PatchTab patchId) pos size =
            HS.text
                [ HSA.translateTo' pos
                , HSA.fill $ Just $ C.toSvg $ C.rgba 0 0 0 1.0
                , HE.onClick $ const $ SelectPatch patchId
                , HSA.dominant_baseline $ HSA.Hanging
                ]
                [ HH.text $ patchId

                ]
        nodeFamily family =
            HS.text
                [ HSA.translateTo' $ 0.0 <+> (font.size * 2.0)
                , HSA.fill $ Just $ C.toSvg $ C.rgba 0 0 0 1.0
                , HE.onClick $ const $ AddNode family
                ]
                [ -- HS.tspan
                    HH.text $ "fff" <> family

                ]
        toolkitInfo toolkit =
            HS.g
                []
                ([ HS.text
                    [ HSA.translateTo' $ 0.0 <+> font.size
                    , HSA.fill $ Just $ C.toSvg $ C.rgba 0 0 0 1.0
                    ]
                    [ -- HS.tspan
                    HH.text $ Toolkit.name toolkit <> ". Families: " <> show (Toolkit.nodeFamiliesCount toolkit)

                    ]
                ] <> (nodeFamily <$> Array.fromFoldable (Toolkit.nodeFamilies toolkit)))


handleAction
    :: forall output patch_action patch_state node_state d m
     . MonadAff m => MonadEffect m
    => Action
    -> H.HalogenM (State patch_state node_state d) Action Slots output m Unit
handleAction = case _ of
    Initialize -> do
        innerWidth <- H.liftEffect $ Window.innerWidth =<< window
        innerHeight <- H.liftEffect $ Window.innerHeight =<< window
        H.modify_ _ { windowSize = toNumber innerWidth <+> toNumber innerHeight }
        -- pure unit
        {- animFrame <- H.liftEffect Emitters.animationFrame
        H.subscribe' $ \sid -> AnimationFrame sid <$> animFrame -}
        windowResize <- H.liftEffect Emitters.windowDimensions
        H.subscribe' $ \sid -> WindowResize sid <$> windowResize
    AddPatch -> do
        H.modify_ \state ->
            let
                newPatch = Patch.empty (Toolkit.state state.toolkit)
                patchCount = Array.length $ Network.patches state.network
                nextPatchId = "patch-" <> show patchCount
            in state
                { network = state.network # Network.addPatch (nextPatchId /\ newPatch)
                , currentPatch = Just nextPatchId
                }
    SelectPatch patchId ->
        H.modify_ _ { currentPatch = Just patchId }
    AddNode family -> do
        let _ = Debug.spy "family" family
        state <- H.get
        maybeNode <- H.liftEffect $ Toolkit.spawn family state.toolkit -- TODO: AndRun
        let _ = Debug.spy "currentPatch" state.currentPatch
        let _ = Debug.spy "maybeNode" maybeNode
        H.put $
            state
                { network =
                    case (/\) <$> state.currentPatch <*> maybeNode of
                        Just (curPatchId /\ node) ->
                            Network.withPatch curPatchId
                                (\patch ->
                                    let nodeCount = Array.length $ Patch.nodes patch
                                        nextNodeId = "node-" <> show nodeCount
                                    in Patch.addNode nextNodeId node patch
                                )
                                state.network
                        Nothing -> state.network
                }
    -- HandlePatch _ ->
    --     H.modify_ \state -> state
    AnimationFrame _ time ->
        H.modify_ _ { currentFrame = time }
    WindowResize _ { w, h } ->
        H.modify_ _ { windowSize = toNumber w <+> toNumber h }
    {-
    ToPatch patchAction ->
        H.tell _tkPatch unit $ ToolkitUI.TellPatch patchAction
    FromPatch (ToolkitUI.Next patchState) ->
        H.modify_ \state -> state
            { patchState = patchState } -}
    Skip ->
        pure unit


component
    :: forall query output patch_state node_state d
     . H.Component query (Input patch_state node_state d) output Aff -- FIXME: there is MonadAff here!
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
        }