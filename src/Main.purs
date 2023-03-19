module Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console

import Control.Monad.State as State

import Data.Maybe (Maybe(..), fromMaybe, maybe, maybe')
import Data.Tuple.Nested ((/\))
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)
import Prim.Symbol (class Append) as S
import Data.Int (floor, toNumber)
import Data.Ord (abs)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Newtype (class Newtype, unwrap)
import Data.Map (Map)
import Data.Map as Map
import Data.Array ((:))
import Data.Array as Array
import Data.Foldable (for_, traverse_)

import Cli.App as Cli

import Blessed ((>~), (~<))
import Blessed (exit) as Blessed
import Blessed as B

import Blessed.Core.Border as Border
import Blessed.Core.Coord ((<+>), (<->))
import Blessed.Core.Coord as Coord
import Blessed.Core.Dimension as Dimension
import Blessed.Core.EndStyle as ES
import Blessed.Core.Key as Key
import Blessed.Core.ListStyle as LStyle
import Blessed.Core.Offset as Offset
import Blessed.Core.Style as Style
import Blessed.Core.Orientation as Orientation

import Blessed.Internal.BlessedSubj (Screen, ListBar, Box, List, Line)
import Blessed.Internal.Core as Core
import Blessed.Internal.JsApi (EventJson)
import Blessed.Internal.BlessedOp (BlessedOpGet, BlessedOp)
import Blessed.Internal.NodeKey (nk, NodeKey(..), type (<^>), RawNodeKey)
import Blessed.Internal.NodeKey as NodeKey
import Blessed.Internal.Emitter (class Fires) as E

import Blessed.UI.Base.Element.Event as Element
import Blessed.UI.Base.Element.Property as Element
import Blessed.UI.Base.Element.PropertySet as Element
import Blessed.UI.Base.Node.Method as Node
import Blessed.UI.Base.Screen as Screen
import Blessed.UI.Base.Screen.Event as Screen
import Blessed.UI.Base.Screen.Method as Screen
import Blessed.UI.Base.Screen.Option as Screen
import Blessed.UI.Boxes.Box as Box
import Blessed.UI.Boxes.Box.Event as Box
import Blessed.UI.Boxes.Box.Method as Box
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Lists.List.Event as List
import Blessed.UI.Lists.List.Option as List
import Blessed.UI.Lists.List.Property as List

import Blessed.UI.Lists.ListBar.Event as ListBar
import Blessed.UI.Lists.ListBar.Option as ListBar
import Blessed.UI.Boxes.Line.Option as Line
import Blessed.UI.Boxes.Line.Event as Line
-- import Blessed.UI.Line.Li ()

import Noodle.Toolkit3 as Toolkit
import Noodle.Network2 as Network


import Toolkit.Hydra2 (toolkit, HydraToolkit, Instances, noInstances) as Hydra


mainScreen = nk :: Screen <^> "main-scr"
patchesBar = nk :: ListBar <^> "patches-bar"
patchBox = nk :: Box <^> "patch-box"
nodeList = nk :: List <^> "node-list"
nodeBox = nk :: Box <^> "node-box"
inletsBar = nk :: ListBar <^> "node-inlets-bar"
outletsBar = nk :: ListBar <^> "node-outlets-bar"
inlets = nk :: ListBar <^> "inlets"
outlets = nk :: ListBar <^> "outlets"


patches = [ "Patch 1", "Patch 2" ]
items = [ "foo", "bar", "buz", "hello", "lalala" ]


type Palette =
    { background :: String
    , background2 :: String
    , border :: String
    , familyMarker :: String
    , focusedBorder :: String
    , foreground :: String
    , itemNotSelected :: String
    , itemSelected :: String
    , linkColor :: String
    , nodeBoxBorder :: String
    , nodeListFg :: String
    , nodeListSelFg :: String
    }


palette :: Palette
palette =
    { background : "#111" -- 0
    , itemNotSelected : "#006600" -- 1
    , itemSelected : "#00ff00" -- 2
    , border : "#f0f0f0" -- 3
    , nodeListFg : "#666" -- 4
    , nodeListSelFg : "white" -- 5
    , nodeBoxBorder : "blue" -- 6
    , familyMarker : "#000033" -- 7
    , linkColor : "green" -- 8
    , focusedBorder : "white"
    , foreground : "white"
    , background2 : "black"
    }


type InletsBarKey = ListBar <^> "node-inlets-bar"
type OutletsBarKey = ListBar <^> "node-outlets-bar"
type NodeBoxKey = Box <^> "node-box"
type PatchBoxKey = Box <^> "patch-box"


-- type Nodes = Hydra.Instances Effect


type State =
    { lastInletsBarKey :: InletsBarKey
    , lastNodeBoxKey :: NodeBoxKey
    , lastOutletsBarKey :: OutletsBarKey
    , lastShiftX :: Int
    , lastShiftY :: Int
    , lastClickedOutlet :: Maybe { node :: NodeBoxKey, index :: Int, subj :: String }
    , lastLink :: Maybe Link
    , linksFrom :: Map RawNodeKey (Map Int Link)
    , linksTo :: Map RawNodeKey (Map Int Link)
    -- , nodes :: Record (Hydra.Instances Effect)
    }


initialState :: State
initialState =
    { lastShiftX : 0
    , lastShiftY : 0
    , lastNodeBoxKey : nodeBox
    , lastInletsBarKey : inletsBar
    , lastOutletsBarKey : outletsBar
    , lastClickedOutlet : Nothing
    , lastLink : Nothing
    , linksFrom : Map.empty
    , linksTo : Map.empty
    -- , nodes : Hydra.noInstances
    }


inletsOutletsStyle =
    List.style
        [ LStyle.bg palette.background
        , LStyle.item
            [ ES.fg palette.itemNotSelected
            , ES.bg palette.background
            ]
        , LStyle.selected
            [ ES.fg palette.itemSelected
            , ES.bg palette.background
            ]
        ]


main1 :: Effect Unit
main1 =
  Cli.run initialState
    (B.screenAnd mainScreen

        [ Screen.title "Noodle"
        , Screen.smartCSR true
        , Screen.fullUnicode true
        , Screen.key
            [ Key.escape, Key.alpha 'q', (Key.control $ Key.alpha 'C') ]
            $ \_ kevt -> do
                Blessed.exit
        ]

        [ B.listbar patchesBar
            [ Box.top $ Offset.px 0
            , Box.left $ Offset.px 0
            , Box.width $ Dimension.percents 100.0
            , Box.height $ Dimension.px 1
            , List.mouse true
            , List.items patches
            , List.style
                [ LStyle.bg palette.background
                , LStyle.item
                    [ ES.fg palette.itemNotSelected
                    , ES.bg palette.background
                    ]
                , LStyle.selected
                    [ ES.fg palette.itemSelected
                    , ES.bg palette.background
                    ]
                ]
            ]
            []

        , B.box patchBox

            [ Box.top $ Offset.calc $ Coord.center <+> Coord.px 1
            , Box.left $ Offset.center
            , Box.width $ Dimension.percents 100.0
            , Box.height $ Dimension.calc $ Coord.percents 100.0 <-> Coord.px 1
            , Box.content "Patch"
            , Box.tags true
            , Box.border
                [ Border.type_ Border._line
                ]
            , Box.style
                [ Style.fg palette.foreground
                , Style.bg palette.background2
                , Style.border [ Border.fg palette.border ]
                ]
            ]

            [ B.listAnd nodeList
                [ Box.top $ Offset.px 0
                , Box.left $ Offset.px 0
                , Box.width $ Dimension.px 14
                , Box.height $ Dimension.percents 40.0
                , Box.draggable true
                , Box.scrollable true
                , List.items items
                , List.mouse true
                , List.keys true
                , Box.border [ Border.type_ Border._line, Border.fg palette.nodeListFg ]
                , List.style
                    [ LStyle.item [ ES.fg palette.nodeListFg ]
                    , LStyle.selected [ ES.fg palette.nodeListSelFg ]
                    ]
                , Core.on List.Select
                    \_ _ -> do
                        -- lastShiftX <- _.lastShiftX <$> State.get
                        -- lastShiftY <- _.lastShiftY <$> State.get
                        -- lastNodeBoxKey <- _.lastNodeBoxKey <$> State.get
                        state <- State.get

                        let top = Offset.px $ state.lastShiftX + 2
                        let left = Offset.px $ 16 + state.lastShiftY + 2
                        let nextNodeBox = NodeKey.next state.lastNodeBoxKey
                        let nextInletsBar = NodeKey.next state.lastInletsBarKey
                        let nextOutletsBar = NodeKey.next state.lastOutletsBarKey

                        selected <- List.selected ~< nodeList
                        -- liftEffect $ Console.log $ show selected

                        let is = [ "a", "b", "c" ]
                        let os = [ "sum", "x" ]

                        let
                            nextNodeBoxN =
                                B.box nextNodeBox
                                    [ Box.draggable true
                                    , Box.top top
                                    , Box.left left
                                    , Box.width $ Dimension.px 25
                                    , Box.height $ Dimension.px 5
                                    , Box.border
                                        [ Border.type_ Border._line
                                        , Border.fg palette.nodeBoxBorder
                                        , Border.ch $ Border.fill ':'
                                        ]
                                    , Box.style
                                        [ Style.focus
                                            [ ES.border
                                                [ Border.fg palette.nodeListSelFg
                                                ]
                                            ]
                                        ]
                                    , Core.on Element.Move $ onNodeMove nextNodeBox -- FIXME: onNodeMove receives wrong `NodeKey` in the handler, probably thanks to `proxies` passed around
                                    ]
                                    [ ]

                        let
                            inletHandler idx iname =
                                iname /\ [] /\ onInletSelect nextNodeBox idx iname
                            inletsBarN =
                                B.listbar nextInletsBar
                                    [ Box.width $ Dimension.percents 90.0
                                    , Box.height $ Dimension.px 1
                                    , Box.top $ Offset.px 0
                                    , Box.left $ Offset.px 0
                                    -- , List.items is
                                    , ListBar.commands $ mapWithIndex inletHandler is
                                    , List.mouse true
                                    , List.keys true
                                    , ListBar.autoCommandKeys true
                                    , inletsOutletsStyle
                                    {- , Core.on ListBar.Select
                                        \_ _ -> do
                                            liftEffect $ Console.log "inlet"
                                            inletSelected <- List.selected ~< nextInletsBar
                                            liftEffect $ Console.log $ show inletSelected
                                    -}
                                    ]
                                    [ ]


                        let
                            outletHandler idx oname =
                                oname /\ [] /\ onOutletSelect nextNodeBox idx oname
                            outletsBarN =
                                B.listbar nextOutletsBar
                                    [ Box.width $ Dimension.percents 90.0
                                    , Box.height $ Dimension.px 1
                                    , Box.top $ Offset.px 2
                                    , Box.left $ Offset.px 0
                                    -- , List.items os
                                    , ListBar.commands $  mapWithIndex outletHandler os
                                    , List.mouse true
                                    , List.keys true
                                    , inletsOutletsStyle
                                    {- , Core.on ListBar.Select
                                        \_ _ -> do
                                            liftEffect $ Console.log "outlet"
                                            outletSelected <- List.selected ~< nextOutletsBar
                                            liftEffect $ Console.log $ show outletSelected
                                    -}
                                    ]
                                    [
                                    ]

                        patchBox >~ Node.append nextNodeBoxN
                        nextNodeBox >~ Node.append inletsBarN
                        nextNodeBox >~ Node.append outletsBarN

                        State.modify_ (_
                            { lastShiftX = state.lastShiftX + 1
                            , lastShiftY = state.lastShiftY + 1
                            , lastNodeBoxKey = nextNodeBox
                            , lastInletsBarKey = nextInletsBar
                            , lastOutletsBarKey = nextOutletsBar
                            } )

                        mainScreen >~ Screen.render

                        pure unit
                ]
                []
                \_ ->
                    pure unit
            ]

        ]


        $ \_ -> do
            nodeList >~ Box.focus
            mainScreen >~ Screen.render
        )

    where

        forgetLink :: Link -> State -> State
        forgetLink link@(Link props) state =
            state
                { linksFrom =
                    Map.update (Map.delete props.id >>> Just) (NodeKey.rawify props.fromNode) state.linksFrom
                , linksTo =
                    Map.update (Map.delete props.id >>> Just) (NodeKey.rawify props.toNode) state.linksTo
                }

        pushLink :: Int -> Link -> Maybe (Map Int Link) -> Maybe (Map Int Link)
        pushLink id link (Just map) = Just $ Map.insert id link map
        pushLink id link Nothing = Just $ Map.singleton id link

        storeLink :: Link -> State -> State
        storeLink link@(Link props) state =
            state
                { linksFrom =
                    Map.alter (pushLink props.id link) (NodeKey.rawify props.fromNode) state.linksFrom
                , linksTo =
                    Map.alter (pushLink props.id link) (NodeKey.rawify props.toNode) state.linksTo
                , lastLink = Just link
                }

        onOutletSelect :: NodeBoxKey -> Int -> String -> OutletsBarKey → EventJson → BlessedOp State Effect
        onOutletSelect onode index oname _ _ = do
            -- liftEffect $ Console.log $ "handler " <> oname
            State.modify_
                (_ { lastClickedOutlet = Just { index, subj : oname, node : onode } })

        onInletSelect :: NodeBoxKey -> Int -> String -> InletsBarKey → EventJson → BlessedOp State Effect
        onInletSelect inode idx iname _ _ = do
            state <- State.get
            -- liftEffect $ Console.log $ "handler " <> iname
            case state.lastClickedOutlet of
                Just lco ->
                    if inode /= lco.node then do
                        link <- createLink
                                    state.lastLink
                                    lco.node
                                    (OutletIndex lco.index)
                                    inode
                                    (InletIndex idx)
                        State.modify_ $ storeLink link
                        patchBox >~ appendLink link
                        link # linkOn Element.Click onLinkClick
                    else pure unit
                Nothing -> pure unit
            State.modify_
                (_ { lastClickedOutlet = Nothing })

        onNodeMove :: NodeBoxKey -> NodeBoxKey → EventJson → BlessedOp State Effect
        onNodeMove nodeKey _ _ = do
            state <- State.get
            let rawNk = NodeKey.rawify nodeKey
            for_ (fromMaybe Map.empty $ Map.lookup rawNk state.linksFrom) updateLink
            for_ (fromMaybe Map.empty $ Map.lookup rawNk state.linksTo) updateLink

        onLinkClick :: forall id. Link -> Line <^> id → EventJson → BlessedOp State Effect
        onLinkClick link _ _ = do
            -- liftEffect $ Console.log "click link"
            patchBox >~ removeLink link
            State.modify_ $ forgetLink link
            mainScreen >~ Screen.render


type LinkLineParams =
    { top :: Int
    , left :: Int
    , width :: Int
    , height :: Int
    }


type NodePositions =
    { fromNodeLeft :: Int
    , fromNodeTop :: Int
    , toNodeLeft :: Int
    , toNodeTop :: Int
    }


type LinkCalc =
    { a :: LinkLineParams
    , b :: LinkLineParams
    , c :: LinkLineParams
    }


newtype Link =
    Link
    { id :: Int
    , blessed :: { a :: Core.Blessed State, b :: Core.Blessed State, c :: Core.Blessed State }
    , fromNode :: NodeBoxKey
    , toNode :: NodeBoxKey
    , outletIndex :: Int
    , inletIndex :: Int
    , keys ::
        { a :: Line <^> "line-a"
        , b :: Line <^> "line-b"
        , c :: Line <^> "line-c"
        }
    }

derive instance Newtype Link _


newtype OutletIndex = OutletIndex Int
newtype InletIndex = InletIndex Int


lineA = nk :: Line <^> "line-a"
lineB = nk :: Line <^> "line-b"
lineC = nk :: Line <^> "line-c"


type LinkHandler = forall id. IsSymbol id => Link -> Line <^> id → EventJson → BlessedOp State Effect


createLink :: Maybe Link -> NodeBoxKey -> OutletIndex -> NodeBoxKey -> InletIndex -> BlessedOpGet State Effect Link
createLink maybePrev fromNode (OutletIndex outletIdx) toNode (InletIndex intletIdx) = do
    fromNodeLeft <- Element.left ~< fromNode
    fromNodeTop <- Element.top ~< fromNode
    toNodeLeft <- Element.left ~< toNode
    toNodeTop <- Element.top ~< toNode
    let

        keyLinkA = fromMaybe lineA $ NodeKey.next <$> _.a <$> _.keys <$> unwrap <$> maybePrev
        keyLinkB = fromMaybe lineB $ NodeKey.next <$> _.b <$> _.keys <$> unwrap <$> maybePrev
        keyLinkC = fromMaybe lineC $ NodeKey.next <$> _.c <$> _.keys <$> unwrap <$> maybePrev
        calc = calcLink { fromNodeLeft, fromNodeTop, toNodeLeft, toNodeTop } (OutletIndex outletIdx) (InletIndex intletIdx)

        -- this.link.a = blessed.line({ left : calc.a.left, top : calc.a.top, width : calc.a.width, height : calc.a.height, orientation : 'vertical', type : 'bg', ch : '≀', fg : PALETTE[8] });
        -- this.link.b = blessed.line({ left : calc.b.left, top : calc.b.top, width : calc.b.width, height : calc.b.height, orientation : 'horizontal', type : 'bg', ch : '∼', fg : PALETTE[8] });
        -- this.link.c = blessed.line({ left : calc.c.left, top : calc.c.top, width : calc.c.width, height : calc.c.height, orientation : 'vertical', type : 'bg', ch : '≀', fg : PALETTE[8] });

        linkA = B.line keyLinkA
                    [ Box.left $ Offset.px calc.a.left
                    , Box.top $ Offset.px calc.a.top
                    , Box.width $ Dimension.px calc.a.width
                    , Box.height $ Dimension.px calc.a.height
                    , Line.orientation $ Orientation.Vertical
                    , Line.ch '≀'
                    , Line.fg $ palette.linkColor
                    ]

        linkB = B.line keyLinkB
                    [ Box.left $ Offset.px calc.b.left
                    , Box.top $ Offset.px calc.b.top
                    , Box.width $ Dimension.px calc.b.width
                    , Box.height $ Dimension.px calc.b.height
                    , Line.orientation $ Orientation.Horizontal
                    , Line.type_ $ Border._bg
                    , Line.ch '∼'
                    , Line.fg $ palette.linkColor
                    ]

        linkC = B.line keyLinkC
                    [ Box.left $ Offset.px calc.c.left
                    , Box.top $ Offset.px calc.c.top
                    , Box.width $ Dimension.px calc.c.width
                    , Box.height $ Dimension.px calc.c.height
                    , Line.orientation $ Orientation.Vertical
                    , Line.type_ $ Border._bg
                    , Line.ch '≀'
                    , Line.fg $ palette.linkColor
                    ]

        link =
            Link
                { id : maybe 0 ((+) 1) $ _.id <$> unwrap <$> maybePrev
                , fromNode
                , toNode
                , outletIndex : outletIdx
                , inletIndex : intletIdx
                , blessed : { a : linkA [], b : linkB [], c : linkC [] }
                , keys : { a : keyLinkA, b : keyLinkB, c : keyLinkC }
                }

    pure link


calcLink :: NodePositions -> OutletIndex -> InletIndex -> LinkCalc
calcLink np (OutletIndex outletIdx) (InletIndex intletIdx) =
    let
        xo = np.fromNodeLeft + (outletIdx * 6)
        yo = np.fromNodeTop + 3
        xi = np.toNodeLeft + (intletIdx * 6)
        yi = np.toNodeTop + 1
        my = floor $ abs (toNumber yi - toNumber yo) / 2.0
        acalc =
            if yo <= yi then -- outlet above inlet
                { left : xo, top : yo, width : 1, height : my }
            else
                { left : xi, top : yi, width : 1, height : my }
        bcalc =
            if yo <= yi then -- outlet above inlet
                if xo <= xi then -- outlet on the left from inlet
                    { left : xo, top : yo + my, width : xi - xo, height : 1 }
                else
                    { left : xi, top : yo + my, width : xo - xi, height : 1 }
            else
                if xi <= xo then -- inlet on the left from outlet
                    { left : xi, top : yi + my, width : xo - xi, height : 1 }
                else
                    { left : xo, top : yi + my, width : xi - xo, height : 1 }
        ccalc =
            if yo <= yi then -- outlet above inlet
                { left : xi, top : yo + my, width : 1, height : my }
            else
                { left : xo, top : yi + my, width : 1, height : my }
    in
    { a : acalc
    , b : bcalc
    , c : ccalc
    }


appendLink :: Link -> PatchBoxKey -> BlessedOp State Effect
appendLink (Link link) pnk = do
    pnk >~ Node.append link.blessed.a
    pnk >~ Node.append link.blessed.b
    pnk >~ Node.append link.blessed.c


removeLink :: Link -> PatchBoxKey -> BlessedOp State Effect
removeLink (Link link) pnk = do
    pnk >~ Node.remove link.blessed.a
    pnk >~ Node.remove link.blessed.b
    pnk >~ Node.remove link.blessed.c



linkOn :: forall e. E.Fires Line e => e -> LinkHandler -> Link -> BlessedOp State Effect
linkOn evt handler (Link link) = do
    link.keys.a >~ Core.on' evt (handler $ Link link)
    link.keys.b >~ Core.on' evt (handler $ Link link)
    link.keys.c >~ Core.on' evt (handler $ Link link)


updateLink :: Link -> BlessedOp State Effect
updateLink (Link link) = do
    fromNodeLeft <- Element.left ~< link.fromNode
    fromNodeTop <- Element.top ~< link.fromNode
    toNodeLeft <- Element.left ~< link.toNode
    toNodeTop <- Element.top ~< link.toNode

    let calc =
            calcLink
            { fromNodeLeft, fromNodeTop, toNodeLeft, toNodeTop }
            (OutletIndex link.outletIndex)
            (InletIndex link.inletIndex)

    link.keys.a >~ Element.setLeft $ Offset.px calc.a.left
    link.keys.a >~ Element.setTop $ Offset.px calc.a.top
    link.keys.a >~ Element.setWidth $ Dimension.px calc.a.width
    link.keys.a >~ Element.setHeight $ Dimension.px calc.a.height

    link.keys.b >~ Element.setLeft $ Offset.px calc.b.left
    link.keys.b >~ Element.setTop $ Offset.px calc.b.top
    link.keys.b >~ Element.setWidth $ Dimension.px calc.b.width
    link.keys.b >~ Element.setHeight $ Dimension.px calc.b.height

    link.keys.c >~ Element.setLeft $ Offset.px calc.c.left
    link.keys.c >~ Element.setTop $ Offset.px calc.c.top
    link.keys.c >~ Element.setWidth $ Dimension.px calc.c.width
    link.keys.c >~ Element.setHeight $ Dimension.px calc.c.height


-- ⊲ ⊳ ⋎ ⋏ ≺ ≻ ⊽ ⋀ ⋁ ∻ ∶ ∼ ∽ ∾ :: ∻ ∼ ∽ ≀ ⊶ ⊷ ⊸ ⋮ ⋯ ⋰ ⋱ ⊺ ⊢ ⊣ ⊤ ⊥ ⊦ ∣ ∤ ∥ ∦ ∗ ∘ ∙ ⋄ ⋅ ⋆ ⋇ > ⋁


main2 :: Effect Unit
main2 =
    let
        lbKey = (nk :: ListBar <^> "test")
        inletHandler iname = iname /\ [ ] /\ \_ _ -> do pure unit -- liftEffect $ Console.log $ "cmd " <> iname
        inletsBarN =
            B.listbar lbKey
                [ Box.width $ Dimension.percents 90.0
                , Box.height $ Dimension.px 1
                , Box.top $ Offset.px 0
                , Box.left $ Offset.px 0
                , ListBar.commands $ inletHandler <$> [ "a", "b", "c" ]
                , List.mouse true
                , List.keys true
                , ListBar.autoCommandKeys true
                , inletsOutletsStyle
                , Core.on ListBar.Select
                    \_ _ -> do
                        -- liftEffect $ Console.log "inlet"
                        inletSelected <- List.selected ~< lbKey
                        -- liftEffect $ Console.log $ show inletSelected
                        pure unit
                -- FIXME: this way it is only possible to assign to one type of events
                -- FIXME: make all B.listBar, B.box methods and so on return object with `Blessed.Event == Blessed.CoreEvent`
                -- FIXME: and/or don't restrict Blessed.* methods to particular events
                , Core.on Element.Click
                    \_ _ -> do
                        liftEffect $ Console.log "preassigned click"
                ]
                [ ]
        nbKey = (nk :: Box <^> "node-box")
        testNodeBox =
            B.box nbKey
                [ Box.draggable true
                , Box.top $ Offset.px 10
                , Box.left $ Offset.px 10
                , Box.width $ Dimension.px 25
                , Box.height $ Dimension.px 5
                , Box.border
                    [ Border.type_ Border._line
                    , Border.fg palette.nodeBoxBorder
                    , Border.ch $ Border.fill ':'
                    ]
                , Box.style
                    [ Style.focus -- FIXME: makes it fail on drag
                        [ ES.border
                            [ Border.fg palette.nodeListSelFg
                            ]
                        ]
                    ]
                    -- []
                ]
                [ ]
  in Cli.run initialState
    (B.screenAnd mainScreen

        [ Screen.title "Noodle"
        , Screen.smartCSR true
        , Screen.fullUnicode true
        , Screen.key
            [ Key.escape, Key.alpha 'q', (Key.control $ Key.alpha 'C') ]
            $ \_ kevt -> do
                Blessed.exit
        ]

        [ inletsBarN
        , testNodeBox
        ]

        $ \_ -> do
            lbKey >~ Core.on' ListBar.Select $ \_ _ -> liftEffect $ Console.log "click assigned after"
            mainScreen >~ Screen.render
    )

-- FIXME: removing links isn't working yet, click works now


main :: Effect Unit
main = main1