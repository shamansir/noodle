module App.Component.App where

import Prelude

import Type.Proxy (Proxy(..))

import Effect.Class (class MonadEffect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)

import Color.Extra as C

import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Data.Vec2 ((<+>), Size)
import Data.Vec2 as V2
import Data.Array (length) as Array
import Data.NonEmpty (NonEmpty, singleton) as NE

import Noodle.Network (Network) as Noodle
import Noodle.Network as Network
import Noodle.Patch as Patch
import Noodle.Toolkit (Toolkit) as Noodle
import Noodle.Toolkit (name, nodeFamilies, nodeFamiliesCount) as Toolkit

import App.Style (Style, NodeFlow)
import App.Style.ClassNames as CS
import App.Component.Patch as PatchC
import App.Emitters as Emitters
import App.Toolkit.UI as ToolkitUI
import App.Svg.Extra as HSA

import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.None as HS
import Halogen.Svg.Attributes as HSA
import Halogen.HTML.CSS as CSS
import CSS as CSS

import Web.HTML (window)
import Web.HTML.Window as Window


type Slots patch_action patch_state =
    ( patch :: PatchC.Slot patch_action Unit
    , tkPatch :: ToolkitUI.PatchSlot' patch_action patch_state Unit
    )


_patch = Proxy :: Proxy "patch"

_tkPatch = Proxy :: Proxy "tkPatch"

_fltc = Proxy :: Proxy "fltc"


type Input patch_action patch_state node_state d =
    { network :: Noodle.Network node_state d
    , toolkit :: Noodle.Toolkit node_state d
    , style :: Style
    , flow :: NodeFlow
    , currentPatch :: Maybe Patch.Id
    , components :: ToolkitUI.Components' patch_action patch_state node_state d
    , markings :: ToolkitUI.Markings
    , getFlags :: ToolkitUI.GetFlags
    , patchState :: patch_state
    }


type State patch_action patch_state node_state d =
    { network :: Noodle.Network node_state d
    , toolkit :: Noodle.Toolkit node_state d
    , style :: Style
    , flow :: NodeFlow
    , currentPatch :: Maybe Patch.Id
    , components :: ToolkitUI.Components' patch_action patch_state node_state d
    , markings :: ToolkitUI.Markings
    , getFlags :: ToolkitUI.GetFlags
    -- ^ same as Input
    , windowSize :: Size
    , currentFrame :: Number
    , patchState :: patch_state
    }


data Action patch_action patch_state
    = Initialize
    | SelectPatch Patch.Id
    | AnimationFrame H.SubscriptionId Number
    | WindowResize H.SubscriptionId { w :: Int, h :: Int }
    | ToPatch patch_action
    | FromPatch (ToolkitUI.InformApp patch_state)
    | Skip
    -- | HandlePatch (PatchC.Action d)


initialState
    :: forall patch_action patch_state d m
     . Input patch_action patch_state d m
    -> State patch_action patch_state d m
initialState { network, toolkit, style, flow, currentPatch, components, markings, getFlags, patchState } =
    { network, toolkit, style, flow, components, markings, getFlags, patchState
    , currentPatch
    , windowSize : 1000.0 <+> 1000.0
    , currentFrame : 0.0
    }


render
    :: forall patch_action patch_state node_state d
     . State patch_action patch_state node_state d
    -> H.ComponentHTML (Action patch_action patch_state) (Slots patch_action patch_state) Aff -- FIXME: there is MonadAff here!
render (s@{ network, toolkit, style, flow }) =
    HH.div
        [ CSS.style $ do
            CSS.fontFamily style.font.family $ NE.singleton CSS.sansSerif
            CSS.fontSize $ CSS.pt style.font.size
        ]
        [ HS.svg
            [ HSA.width $ V2.w s.windowSize, HSA.height $ V2.h s.windowSize
            , HSA.id "noodle"
            ]
            [ background
            , curFrame
            , patchesTabs
            , maybeCurrent currentPatch
            ]
        ]
    where
        currentPatch = (flip Network.patch $ network) =<< s.currentPatch
        curFrame =
            HS.text
                [ HSA.translateTo' $ 200.0 <+> 0.0
                , HSA.class_ $ H.ClassName "debug"
                ]
                [ HH.text $ show s.currentFrame ]
        tabHeight = 20.0
        tabPadding = 4.0
        tabLength = 60.0
        patchOffset = 0.0 <+> (tabHeight + tabPadding)
        background =
            HS.g
                []
                [ HS.rect
                    [ HSA.width $ V2.w s.windowSize, HSA.height $ V2.h s.windowSize
                    , HSA.fill $ Just $ C.toSvg style.bg.fill
                    ]
                , case (/\) <$> currentPatch <*> s.components.patch of
                    Nothing ->
                        HS.none
                    Just (patch /\ tkPatchComp) ->
                        HH.slot
                            _tkPatch
                            unit
                            tkPatchComp
                            { patch, size : s.windowSize, patchState : s.patchState }
                            FromPatch
                ]
        patchesTabs | (Array.length $ Network.patches network) > 0 =
                        HS.g [ HSA.classes CS.patchesTabs ] (patchTab <$> Tuple.fst <$> Network.patches network)
        patchesTabs | otherwise =
                        HS.text
                            [ HSA.translateTo' $ 0.0 <+> tabHeight ]
                            [ HH.text "No patches in network." ]
        patchTab label =
            HS.g
                [ HSA.classes $ CS.patchTab label ]
                [ HS.rect [ HSA.width tabLength, HSA.height tabHeight
                , HSA.fill $ Just $ C.toSvg style.patchTab.background
                , HSA.stroke $ Just $ C.toSvg style.patchTab.stroke
                , HSA.strokeWidth 1.0
                ]
                , HS.text
                    [ HSA.translateTo' $ 3.0 <+> 3.0 ]
                    [ HH.text label ]
                ]
        maybeCurrent (Just patch) =
            HS.g
                [ HSA.translateTo' $ 0.0 <+> tabHeight + tabPadding ]
                [ HH.slot _patch unit
                    PatchC.component
                    { patch, toolkit, style, flow
                    , offset : patchOffset
                    , markings : s.markings, getFlags : s.getFlags
                    , customNodeBody : s.components.node
                    , area : s.windowSize - patchOffset
                    , patchState : s.patchState
                    }
                    ToPatch
                ]
        maybeCurrent Nothing =
            HS.g
                []
                [ HS.text
                        [ HSA.translateTo' $ 0.0 <+> tabHeight + tabPadding + 12.0 ]
                        [ HH.text "No patch selected" ]
                , toolkitInfo toolkit
                ]
        toolkitInfo toolkit =
            HS.g
                []
                [ HS.text
                    [ HSA.translateTo' $ 0.0 <+> tabHeight + tabPadding + 40.0 ]
                    [ -- HS.tspan
                    HH.text $ Toolkit.name toolkit <> ". Families: " <> show (Toolkit.nodeFamiliesCount toolkit)

                    ]
                ]


handleAction
    :: forall output patch_action patch_state node_state d m
     . MonadAff m => MonadEffect m
    => Action patch_action patch_state
    -> H.HalogenM (State patch_action patch_state node_state d) (Action patch_action patch_state) (Slots patch_action patch_state) output m Unit
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
    SelectPatch _ ->
        H.modify_ \state -> state
    -- HandlePatch _ ->
    --     H.modify_ \state -> state
    AnimationFrame _ time ->
        H.modify_ \state -> state
            { currentFrame = time }
    WindowResize _ { w, h } ->
        H.modify_ \state -> state
            { windowSize = toNumber w <+> toNumber h }
    ToPatch patchAction ->
        H.tell _tkPatch unit $ ToolkitUI.TellPatch patchAction
    FromPatch (ToolkitUI.Next patchState) ->
        H.modify_ \state -> state
            { patchState = patchState }
    Skip ->
        pure unit


component
    :: forall query output patch_action patch_state node_state d
     . H.Component query (Input patch_action patch_state node_state d) output Aff -- FIXME: there is MonadAff here!
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
        }