module App.Component.Network where

import Prelude

import Type.Proxy (Proxy(..))

import Effect.Class (class MonadEffect)
import Effect.Aff.Class (class MonadAff)
import Data.Unit (Unit, unit)
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))

import Noodle.Network (Network) as Noodle
import Noodle.Network as Network
import Noodle.Toolkit (Toolkit) as Noodle

import App.Colors as Colors
import App.ClassNames as CS
import App.Component.Patch as PatchC
import App.Mouse as Mouse
import App.Emitters as Emitters

import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HSA
import Halogen.Query.Event (eventListener)

import Web.HTML (window)
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes as MET
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window


type Slots = ( patch :: PatchC.Slot Unit )


_patch = Proxy :: Proxy "patch"


type Input d =
    { nw :: Noodle.Network d
    , toolkit :: Noodle.Toolkit d
    }


type State d =
    { nw :: Noodle.Network d
    , toolkit :: Noodle.Toolkit d
    , currentPatch :: Maybe String
    , width :: Int, height :: Int
    , mouse :: Mouse.State Unit
    , currentFrame :: Number
    }


data Action d
    = Initialize
    | SelectPatch String
    | HandleMouse H.SubscriptionId ME.MouseEvent
    | AnimationFrame H.SubscriptionId Number
    | WindowResize H.SubscriptionId { w :: Int, h :: Int }
    | HandlePatch (PatchC.Action d)


initialState :: forall d. Input d -> State d
initialState { nw, toolkit } =
    { nw, toolkit
    , currentPatch : Just "base"
    , width : 1000, height : 1000
    , mouse : Mouse.init
    , currentFrame : 0.0
    }


render :: forall d m. MonadEffect m => State d -> H.ComponentHTML (Action d) Slots m
render (s@{ nw, toolkit }) =
    HS.svg
        [ HSA.width $ toNumber s.width, HSA.height $ toNumber s.height ]
        [ background
        , mouseState
        , curFrame
        , patchesTabs
        , maybeCurrent $ (flip Network.patch $ nw) =<< s.currentPatch
        ]
    where
        mouseState =
            HS.text
                [ HSA.transform [ HSA.Translate 100.0 0.0 ] ]
                [ HH.text $ show s.mouse ]
        curFrame =
            HS.text
                [ HSA.transform [ HSA.Translate 200.0 0.0 ] ]
                [ HH.text $ show s.currentFrame ]
        tabHeight = 20.0
        tabLength = 60.0
        background =
            HS.rect
                [ HSA.width $ toNumber s.width, HSA.height $ toNumber s.height
                , HSA.fill $ Just Colors.background
                ]
        patchesTabs = HS.g [ HSA.classes CS.patchesTabs ] (patchTab <$> Tuple.fst <$> Network.patches nw)
        patchTab label =
            HS.g
                [ HSA.classes CS.patchTab ]
                [ HS.rect [ HSA.width tabLength, HSA.height tabHeight, HSA.fill $ Just Colors.tabBackground ]
                , HS.text [] [ HH.text label ]
                ]
        maybeCurrent (Just patch) =
            HS.g
                [ HSA.transform [ HSA.Translate 0.0 tabHeight ] ]
                [ HH.slot _patch unit PatchC.component { patch, toolkit } absurd ]
        maybeCurrent Nothing =
            HS.text
                [ HSA.transform [ HSA.Translate 0.0 tabHeight ] ]
                [ HH.text "No patch selected" ]


handleAction :: forall output m d. MonadAff m => MonadEffect m => Action d -> H.HalogenM (State d) (Action d) Slots output m Unit
handleAction = case _ of
    Initialize -> do
        -- pure unit
        innerWidth <- H.liftEffect $ Window.innerWidth =<< window
        innerHeight <- H.liftEffect $ Window.innerWidth =<< window
        H.modify_ _ { width = innerWidth, height = innerHeight }
        document <- H.liftEffect $ document =<< window
        H.subscribe' $ Emitters.mouseDown document <<< HandleMouse
        H.subscribe' $ Emitters.mouseMove document <<< HandleMouse
        H.subscribe' $ Emitters.mouseUp document <<< HandleMouse
        {- animFrame <- H.liftEffect Emitters.animationFrame
        H.subscribe' $ \sid -> AnimationFrame sid <$> animFrame -}
        windowResize <- H.liftEffect Emitters.windowDimensions
        H.subscribe' $ \sid -> WindowResize sid <$> windowResize
    SelectPatch _ ->
        H.modify_ \state -> state
    HandlePatch _ ->
        H.modify_ \state -> state
    HandleMouse _ mouseEvent ->
        H.modify_ \state -> state
            { mouse = state.mouse # Mouse.apply (const $ Just unit) mouseEvent }
    AnimationFrame _ time ->
        H.modify_ \state -> state
            { currentFrame = time }
    WindowResize _ { w, h } ->
        H.modify_ \state -> state
            { width = w, height = h }


component :: forall query output m d. MonadAff m => MonadEffect m => H.Component query (Input d) output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
        }