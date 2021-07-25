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
import Data.Vec2 ((<+>))

import Noodle.Network (Network) as Noodle
import Noodle.Network as Network
import Noodle.Toolkit (Toolkit) as Noodle

import App.Style (Style, NodeFlow(..))
import App.Style.ClassNames as CS
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
    , style :: Style
    , flow :: NodeFlow
    }


type State d =
    { nw :: Noodle.Network d
    , toolkit :: Noodle.Toolkit d
    , currentPatch :: Maybe String
    , width :: Int, height :: Int
    , currentFrame :: Number
    , style :: Style
    , flow :: NodeFlow
    }


data Action d
    = Initialize
    | SelectPatch String
    | AnimationFrame H.SubscriptionId Number
    | WindowResize H.SubscriptionId { w :: Int, h :: Int }
    | HandlePatch (PatchC.Action d)


initialState :: forall d. Input d -> State d
initialState { nw, toolkit, style, flow } =
    { nw, toolkit, style, flow
    , currentPatch : Just "base"
    , width : 1000, height : 1000
    , currentFrame : 0.0
    }


render :: forall d m. MonadEffect m => State d -> H.ComponentHTML (Action d) Slots m
render (s@{ nw, toolkit, style, flow }) =
    HS.svg
        [ HSA.width $ toNumber s.width, HSA.height $ toNumber s.height
        , HSA.id "noodle"
        ]
        [ background
        , curFrame
        , patchesTabs
        , maybeCurrent $ (flip Network.patch $ nw) =<< s.currentPatch
        ]
    where
        colors = style.colors
        curFrame =
            HS.text
                [ HSA.transform [ HSA.Translate 200.0 0.0 ] ]
                [ HH.text $ show s.currentFrame ]
        tabHeight = 20.0
        tabPadding = 4.0
        tabLength = 60.0
        patchOffset = 0.0 <+> (tabHeight + tabPadding)
        background =
            HS.rect
                [ HSA.width $ toNumber s.width, HSA.height $ toNumber s.height
                , HSA.fill $ Just colors.background
                ]
        patchesTabs = HS.g [ HSA.classes CS.patchesTabs ] (patchTab <$> Tuple.fst <$> Network.patches nw)
        patchTab label =
            HS.g
                [ HSA.classes $ CS.patchTab label ]
                [ HS.rect [ HSA.width tabLength, HSA.height tabHeight
                , HSA.fill $ Just colors.patchTabBackground
                , HSA.stroke $ Just colors.patchTabStroke
                , HSA.strokeWidth 1.0
                ]
                , HS.text
                    [ HSA.transform [ HSA.Translate 3.0 3.0 ] ]
                     [ HH.text label ]
                ]
        maybeCurrent (Just patch) =
            HS.g
                [ HSA.transform [ HSA.Translate 0.0 $ tabHeight + tabPadding ] ]
                [ HH.slot _patch unit PatchC.component { patch, toolkit, style, flow, offset : patchOffset } absurd ]
        maybeCurrent Nothing =
            HS.text
                [ HSA.transform [ HSA.Translate 0.0 $ tabHeight + tabPadding ] ]
                [ HH.text "No patch selected" ]


handleAction :: forall output m d. MonadAff m => MonadEffect m => Action d -> H.HalogenM (State d) (Action d) Slots output m Unit
handleAction = case _ of
    Initialize -> do
        innerWidth <- H.liftEffect $ Window.innerWidth =<< window
        innerHeight <- H.liftEffect $ Window.innerWidth =<< window
        H.modify_ _ { width = innerWidth, height = innerHeight }
        -- pure unit
        {- animFrame <- H.liftEffect Emitters.animationFrame
        H.subscribe' $ \sid -> AnimationFrame sid <$> animFrame -}
        windowResize <- H.liftEffect Emitters.windowDimensions
        H.subscribe' $ \sid -> WindowResize sid <$> windowResize
    SelectPatch _ ->
        H.modify_ \state -> state
    HandlePatch _ ->
        H.modify_ \state -> state
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