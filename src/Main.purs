module Main where

import Prelude


import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console

import Cli.App as Cli

import Blessed ((>~))
import Blessed as B
import Blessed (exit) as Blessed
import Type.Proxy (Proxy(..))

import Blessed.Core.Key as Key
import Blessed.Core.Offset as Offset
import Blessed.Core.EndStyle as ES
import Blessed.Core.Border as Border
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Style as Style
import Blessed.Core.Coord ((<+>), (<->))
import Blessed.Core.Coord as Coord
import Blessed.Core.ListStyle as LStyle

import Blessed.Internal.BlessedSubj (Screen, ListBar, Box, List)
import Blessed.Internal.NodeKey (nk, NodeKey(..), type (<^>))
import Blessed.Internal.NodeKey as NodeKey
import Blessed.Internal.Core as Core


import Blessed.UI.Boxes.Box as Box
import Blessed.UI.Base.Element.Event as Element
import Blessed.UI.Base.Screen as Screen
import Blessed.UI.Base.Screen.Option as Screen
import Blessed.UI.Base.Screen.Event as Screen
import Blessed.UI.Base.Screen.Method as Screen
import Blessed.UI.Base.Node.Method as Node
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Event as Box
import Blessed.UI.Boxes.Box.Method as Box
import Blessed.UI.Lists.List.Option as List
import Blessed.UI.Lists.List.Event as List
import Blessed.UI.Lists.List.Property as List

import Control.Monad.State as State


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


main :: Effect Unit
main = do
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
                                            liftEffect $ Console.log "move"
                                    ]
                                    [ ]

                        let
                            inletsBarN =
                                B.listbar nextInletsBar
                                    [ Box.width $ Dimension.percents 90.0
                                    , Box.height $ Dimension.px 1
                                    , Box.top $ Offset.px 0
                                    , Box.left $ Offset.px 0
                                    , List.items is
                                    , List.mouse true
                                    , List.keys true
                                    , inletsOutletsStyle
                                    -- , Core.on List.Select
                                    --     \_ _ ->
                                    --         liftEffect $ Console.log "intlet"
                                    ]
                                    [ ]


                        let
                            outletsBarN =
                                B.listbar nextOutletsBar
                                    [ Box.width $ Dimension.percents 90.0
                                    , Box.height $ Dimension.px 1
                                    , Box.top $ Offset.px 2
                                    , Box.left $ Offset.px 0
                                    , List.items os
                                    , List.mouse true
                                    , List.keys true
                                    , inletsOutletsStyle
                                    , Core.on List.Select
                                        \_ _ ->
                                            liftEffect $ Console.log "outlet"
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
