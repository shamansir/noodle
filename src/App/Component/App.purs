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
-- import App.Component.Patch as PatchC
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
    , toolkit :: Noodle.Toolkit node_state d
    , currentPatch :: Maybe Patch.Id
    , markings :: ToolkitUI.Markings
    , getFlags :: ToolkitUI.GetFlags
    , patchState :: patch_state
    }


type State patch_state node_state d =
    { network :: Noodle.Network d
    , toolkit :: Noodle.Toolkit node_state d
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
render (s@{ network, toolkit }) =
    HH.div
        [ CSS.style $ do
            CSS.fontFamily font.family $ NE.singleton CSS.sansSerif
            CSS.fontSize $ CSS.pt font.size
        ]
        [ toolkitInfo toolkit
        ]
    where
        toolkitInfo toolkit =
            HS.g
                []
                [ HS.text
                    [ ]
                    [ -- HS.tspan
                    HH.text $ Toolkit.name toolkit <> ". Families: " <> show (Toolkit.nodeFamiliesCount toolkit)

                    ]
                ]


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