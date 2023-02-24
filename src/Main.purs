module Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console

import Control.Monad.State as State

import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple.Nested ((/\))
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)
import Prim.Symbol (class Append) as S

import Cli.App as Cli

import Blessed ((>~))
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

import Blessed.Internal.BlessedSubj (Screen, ListBar, Box, List, Line)
import Blessed.Internal.Core as Core
import Blessed.Internal.BlessedOp (BlessedOpGet, BlessedOp)
import Blessed.Internal.NodeKey (nk, NodeKey(..), type (<^>), RawNodeKey)
import Blessed.Internal.NodeKey as NodeKey

import Blessed.UI.Base.Element.Event as Element
import Blessed.UI.Base.Element.Property as Element
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
-- import Blessed.UI.Line.Li ()



mainScreen = nk :: Screen <^> "main-scr"
patchesBar = nk :: ListBar <^> "patches-bar"
patchBox = nk :: Box <^> "patch-box"
nodeList = nk :: List <^> "node-list"
nodeBox = nk :: Box <^> "node-box"
inletsBar = nk :: ListBar <^> "node-inlets-bar"
outletsBar = nk :: ListBar <^> "node-outlets-bar"
inlets = nk :: ListBar <^> "inlets"
outlets = nk :: ListBar <^> "outlets"


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


patches =
    [ "Patch1", "Patch2", "+" ]


items =
    [ "foo", "bar", "ololo", "hello", "foo1", "bar1", "ololo1", "hello1", "foo2", "bar2", "ololo2", "hello2" ]


initialState =
    { lastShiftX : 0
    , lastShiftY : 0
    , lastNodeBoxKey : nodeBox
    , lastInletsBarKey : inletsBar
    , lastOutletsBarKey : outletsBar
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
main1 = do
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

                        -- TODO: inverse operator for (>~) : selected <- List.selected ~< nodeList
                        selected <- List.selected nodeList
                        liftEffect $ Console.log $ show selected

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
                                    , Core.on Element.Move
                                        \_ _ ->
                                            -- liftEffect $ Console.log "move"
                                            pure unit
                                    ]
                                    [ ]

                        let
                            inletHandler iname = iname /\ [] /\ \_ _ -> do liftEffect $ Console.log $ "handler " <> iname
                            inletsBarN =
                                B.listbar nextInletsBar
                                    [ Box.width $ Dimension.percents 90.0
                                    , Box.height $ Dimension.px 1
                                    , Box.top $ Offset.px 0
                                    , Box.left $ Offset.px 0
                                    -- , List.items is
                                    , ListBar.commands $ inletHandler <$> is
                                    , List.mouse true
                                    , List.keys true
                                    , ListBar.autoCommandKeys true
                                    , inletsOutletsStyle
                                    , Core.on ListBar.Select
                                        \_ _ -> do
                                            liftEffect $ Console.log "inlet"
                                            inletSelected <- List.selected nextInletsBar
                                            liftEffect $ Console.log $ show inletSelected
                                    ]
                                    [ ]


                        let
                            outletHandler oname = oname /\ [] /\ \_ _ -> do liftEffect $ Console.log $ "handler " <> oname
                            outletsBarN =
                                B.listbar nextOutletsBar
                                    [ Box.width $ Dimension.percents 90.0
                                    , Box.height $ Dimension.px 1
                                    , Box.top $ Offset.px 2
                                    , Box.left $ Offset.px 0
                                    -- , List.items os
                                    , ListBar.commands $ outletHandler <$> os
                                    , List.mouse true
                                    , List.keys true
                                    , inletsOutletsStyle
                                    , Core.on ListBar.Select
                                        \_ _ -> do
                                            liftEffect $ Console.log "outlet"
                                            outletSelected <- List.selected nextOutletsBar
                                            liftEffect $ Console.log $ show outletSelected
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


type Link state e =
    { blessed :: { a :: Core.Blessed state e, b :: Core.Blessed state e, c :: Core.Blessed state e }
    , fromNode :: NodeKey Box "node-box"
    , toNode :: NodeKey Box "node-box"
    , outletIndex :: Int
    , inletIndex :: Int
    , keys :: { a :: NodeKey Line "line-a", b :: NodeKey Line "line-b", c :: NodeKey Line "line-c" }
    }


newtype OutletIndex = OutletIndex Int
newtype InletIndex = InletIndex Int


lineA = nk :: Line <^> "line-a"
lineB = nk :: Line <^> "line-b"
lineC = nk :: Line <^> "line-c"


calcLink :: NodePositions -> OutletIndex -> InletIndex -> LinkCalc
calcLink np (OutletIndex outletIdx) (InletIndex intletIdx) =
    { a : { top : 0, left : 0, width : 0, height : 0 }
    , b : { top : 0, left : 0, width : 0, height : 0 }
    , c : { top : 0, left : 0, width : 0, height : 0 }
    }


createLink :: forall state e. Maybe (Link state e) -> NodeKey Box "node-box" -> OutletIndex -> NodeKey Box "node-box" -> InletIndex -> BlessedOpGet state Effect (Link state e)
createLink maybePrev fromNode (OutletIndex outletIdx) toNode (InletIndex intletIdx) = do
    fromNodeLeft <- Element.boxLeft fromNode
    fromNodeTop <- Element.boxTop fromNode
    toNodeLeft <- Element.boxLeft toNode
    toNodeTop <- Element.boxTop toNode
    let
        keyLinkA = fromMaybe lineA $ NodeKey.next <$> _.a <$> _.keys <$> maybePrev
        keyLinkB = fromMaybe lineB $ NodeKey.next <$> _.b <$> _.keys <$> maybePrev
        keyLinkC = fromMaybe lineC $ NodeKey.next <$> _.c <$> _.keys <$> maybePrev
        calc = calcLink { fromNodeLeft, fromNodeTop, toNodeLeft, toNodeTop } (OutletIndex outletIdx) (InletIndex intletIdx)
        linkA = B.line keyLinkA [] []
        linkB = B.line keyLinkB [] []
        linkC = B.line keyLinkC [] []
    pure
        { fromNode
        , toNode
        , outletIndex : outletIdx
        , inletIndex : intletIdx
        , blessed : { a : linkA, b : linkB, c : linkC }
        , keys : { a : keyLinkA, b : keyLinkB, c : keyLinkC }
        }


appendLink :: forall state e m. Link state e -> NodeKey Box "patch-box" -> BlessedOp state m
appendLink link pnk = do
    pnk >~ Node.append link.blessed.a
    pnk >~ Node.append link.blessed.b
    pnk >~ Node.append link.blessed.c


main2 :: Effect Unit
main2 = do
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

        [
            let
                lbKey = (nk :: ListBar <^> "test")
                inletHandler iname = iname /\ [ ] /\ \_ _ -> do liftEffect $ Console.log $ "cmd " <> iname
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
                                liftEffect $ Console.log "inlet"
                                inletSelected <- List.selected lbKey
                                liftEffect $ Console.log $ show inletSelected
                        ]
                        [ ]
            in inletsBarN
        ]

        $ \_ -> do
            mainScreen >~ Screen.render
    )


main :: Effect Unit
main = main1