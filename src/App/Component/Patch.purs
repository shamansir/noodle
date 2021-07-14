module App.Component.Patch where


import Prelude

import Data.Unit (Unit, unit)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.List as List
import Data.Array as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe as Maybe
import Data.Tuple (curry, uncurry)
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.PinBoard (PinBoard)
import Data.PinBoard as PB

import Control.Alternative ((<|>))

import Effect.Class (class MonadEffect, liftEffect)

import Data.BinPack.R2 (Bin2)
import Data.BinPack.R2 as R2

import Noodle.Patch (Patch) as Noodle
import Noodle.Patch as Patch
import Noodle.Toolkit (Toolkit) as Noodle
import Noodle.Toolkit as Toolkit

import App.Component.Node as NodeC
import App.Style (Style, NodeFlow(..))
import App.Style as Style
import App.Style.ClassNames as CS
import App.Style.Calculate as Calc
import App.Mouse as Mouse
import App.Emitters as Emitters

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HSA

import Web.HTML (window)
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes as MET
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window


import Type.Proxy (Proxy(..))


type Slot id = forall query. H.Slot query Void id


type Slots = ( node :: NodeC.Slot Int )


_node = Proxy :: Proxy "node"


type Input d =
    { patch :: Noodle.Patch d
    , toolkit :: Noodle.Toolkit d
    , style :: Style
    , flow :: NodeFlow
    , offset :: Number /\ Number
    }


type State d =
    { patch :: Noodle.Patch d
    , toolkit :: Noodle.Toolkit d
    , style :: Style
    , flow :: NodeFlow
    , offset :: Number /\ Number
    , layout :: Bin2 Number String
    , pinned :: PinBoard String
    , mouse :: Mouse.State String
    }


data Action d
    = Initialize
    | Receive (Input d)
    | AddNode String
    | HandleMouse H.SubscriptionId ME.MouseEvent


initialState :: forall d. Input d -> State d
initialState { patch, toolkit, style, flow, offset } =
    { patch, toolkit, style, flow
    , offset : case offset of (x /\ y) -> (x /\ (y + tabHeight + tabVertPadding))
    , layout : R2.container 1500.0 900.0
    , pinned : []
    , mouse : Mouse.init
    }


tabHeight = 20.0
tabVertPadding = 15.0
tabHorzPadding = 5.0
tabLength = 60.0


render :: forall d m. State d -> H.ComponentHTML (Action d) Slots m
render { patch, toolkit, layout, style, flow, mouse, offset } =
    HS.g
        []
        [ mouseState
        , nodeButtons
        , nodesLayout
        , pinnedNodes
        , maybeDraggedNode mouse
        ]
    where
        mouseState =
            HS.text
                [ HSA.transform [ HSA.Translate 100.0 0.0 ]
                , HSA.fill $ Just $ Style.white
                ]
                [ HH.text $ show $ Mouse.shift offset mouse ]
        colors = style.colors
        packedNodes
            = List.catMaybes
            $ (\(name /\ x /\ y /\ w /\ h) ->
                patch
                     #  Patch.findNode name
                    <#> { name, node : _, x, y, w, h }
            ) <$> R2.toList layout
        nodeButtons = HS.g [ HSA.classes CS.nodesTabs ] $ nodeButton <$> (Set.toUnfoldable $ Toolkit.nodeNames toolkit)
        nodeButton name =
            HS.g
                [ HSA.classes $ CS.nodeButton name
                , HSA.transform [ HSA.Translate tabHorzPadding 0.0 ]
                , HE.onClick \_ -> AddNode name
                ]
                [ HS.rect
                    [ HSA.width tabLength, HSA.height tabHeight
                    , HSA.fill $ Just colors.nodeTabBackground
                    , HSA.stroke $ Just colors.nodeTabStroke
                    , HSA.strokeWidth 1.0
                    ]
                , HS.text [] [ HH.text $ "+ " <> name ]
                ]
        node' idx { node, name, x, y, w, h } =
            HS.g
                [ HSA.transform [ HSA.Translate x $ tabHeight + tabVertPadding + y ]
                , HSA.classes $ CS.node flow name
                ]
                [ HH.slot _node idx NodeC.component { node, name, style, flow } absurd ]
        nodesLayout =
            HS.g [ HSA.classes CS.nodes ] $ Array.mapWithIndex node' $ List.toUnfoldable $ packedNodes -- Patch.nodes patch
        pinnedNodes =
            HS.g [] []
        maybeDraggedNode (Mouse.Dragging _ (x /\ y) _) =
            HS.g
                [ HSA.transform [ HSA.Translate x y ] ]
                [ HS.text [] [ HH.text "aaa" ] ]
        maybeDraggedNode _ =
            HS.g [] []



handleAction :: forall output m d. MonadEffect m => Action d -> H.HalogenM (State d) (Action d) Slots output m Unit
handleAction = case _ of

    Initialize -> do
        document <- H.liftEffect $ document =<< window
        H.subscribe' $ Emitters.mouseDown document <<< HandleMouse
        H.subscribe' $ Emitters.mouseMove document <<< HandleMouse
        H.subscribe' $ Emitters.mouseUp document <<< HandleMouse

    Receive _ ->
        pure unit
        --H.modify_ (\state -> state { patch = input.patch })

    AddNode name -> do
        toolkit <- H.gets _.toolkit
        case Toolkit.spawn name toolkit of
            Just newNode' -> do
                newNode <- liftEffect newNode'
                H.modify_ -- _ { patch = _.patch # Patch.addNode "sum" newNode }
                    (\state ->
                        let nodeName = makeUniqueName state.patch name
                            flow = Vertical -- FIXME
                            width /\ height =
                                Calc.nodeBounds (state.style.units flow) flow newNode
                        in state
                            { patch = state.patch # Patch.addNode nodeName newNode
                            , layout = R2.packOne state.layout (R2.item width height nodeName)
                                        # Maybe.fromMaybe state.layout
                            }
                    )
            Nothing -> pure unit
        where makeUniqueName patch name = name <> "-" <> (show $ Patch.nodesCount patch + 1)

    HandleMouse _ mouseEvent ->
        H.modify_ \state ->
            let
                nextMouse
                    = state.mouse
                        # Mouse.apply (findNode state)
                        mouseEvent
            in case nextMouse of
                Mouse.StartDrag _ i ->
                    state
                        { mouse =
                            nextMouse
                        , layout =
                            state.layout # R2.abandon i -- FIXME: abandon when we started to drag it
                        }
                Mouse.DropAt pos i ->
                    let
                        flow = Vertical -- FIXME
                        width /\ height =
                            state.patch
                                # Patch.findNode i
                                # map (Calc.nodeBounds (state.style.units flow) flow)
                                # Maybe.fromMaybe (0.0 /\ 0.0)
                    in
                        state
                            { mouse =
                                nextMouse
                            , pinned =
                                state.pinned # PB.pin pos (width /\ height) i
                            }
                _ ->
                    state
                        { mouse =
                            nextMouse
                        }

    where
        findNode state pos =
            findInLayout state pos <|> findInPinned state pos
        findInLayout state =
            Mouse.shift' state.offset
                >>> (uncurry $ R2.sample' state.layout)
        findInPinned state =
            Mouse.shift' state.offset
                >>> flip PB.search state.pinned


component :: forall query output m d. MonadEffect m => H.Component query (Input d) output m
component =
    H.mkComponent
        { initialState
        , render
        , eval:
            H.mkEval H.defaultEval
                { handleAction = handleAction
                , receive = Just <<< Receive
                , initialize = Just Initialize
                }
        }