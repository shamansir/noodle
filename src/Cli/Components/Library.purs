module Cli.Components.Library where


import Control.Monad.State as State

import Data.Maybe (Maybe(..))
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\))
import Data.Array ((!!))
import Data.Mark (mark)

import Blessed as B
import Blessed ((>~), (~<))

import Blessed.Core.Offset as Offset
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Border as Border
import Blessed.Core.ListStyle as LStyle
import Blessed.Core.EndStyle as ES

import Blessed.Internal.Core as Core
import Blessed.Internal.NodeKey as NodeKey

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Lists.List.Event (ListEvent(..)) as List
import Blessed.UI.Lists.List.Option (items, keys, mouse, style) as List
import Blessed.UI.Lists.List.Property (selected) as List


import Noodle.Id as Id
import Noodle.Network2 as Network

import Cli.Keys as Key
import Cli.Palette (palette)
import Cli.Palette.Item (repr)
import Cli.State (State)
import Cli.State.NwWraper (unwrapN)

import Cli.Components.NodeBox as NodeBox

import Toolkit.Hydra2 as Hydra
import Toolkit.Hydra2.Group as Hydra

import Blessed.Tagger (fgc, s, render) as T



import Prelude


component :: Array Id.FamilyR -> Core.Blessed State
component families =
    B.listAnd Key.nodeList
        [ Box.top $ Offset.px 0
        , Box.left $ Offset.px 0
        , Box.width $ Dimension.px 14
        , Box.height $ Dimension.percents 40.0
        , Box.draggable true
        , Box.scrollable true
        , List.items $ toItem <$> families
        , List.mouse true
        , List.keys true
        , Box.tags true
        , Box.border [ Border.type_ Border._line, Border.fg $ repr palette.nodeListFg ]
        , List.style
            [ LStyle.item [ ES.fg $ repr palette.nodeListFg ]
            , LStyle.selected [ ES.fg $ repr palette.nodeListSelFg ]
            ]
        , Core.on List.Select
            \_ _ -> do
                -- lastShiftX <- _.lastShiftX <$> State.get
                -- lastShiftY <- _.lastShiftY <$> State.get
                -- lastNodeBoxKey <- _.lastNodeBoxKey <$> State.get
                state <- State.get

                {- -}
                let mbCurrentPatchId = Tuple.snd <$> state.currentPatch
                let mbCurrentPatch = mbCurrentPatchId >>= \id -> Network.patch id $ unwrapN state.network
                {- -}
                -- patchesBar >~ ListBar.addItemH ?wh [] ?wh

                -- Hydra.withFamily

                let top = Offset.px $ state.lastShiftX + 2
                let left = Offset.px $ 16 + state.lastShiftY + 2
                let nextNodeBox = NodeKey.next state.lastNodeBoxKey
                let nextInletsBar = NodeKey.next state.lastInletsBarKey
                let nextOutletsBar = NodeKey.next state.lastOutletsBarKey

                {- -}
                selected <- List.selected ~< Key.nodeList
                let mbSelectedFamily = families !! selected

                {-
                let familyStr = fromMaybe "??" (Id.reflect' <$> mbSelectedFamily)

                Key.patchesBar >~ ListBar.setItems
                    [ "test1" /\ [] /\ \_ _ -> do liftEffect $ Console.log "foo"
                    , "test2" /\ [] /\ \_ _ -> do liftEffect $ Console.log "bar"
                    , familyStr /\ [] /\ \_ _ -> do liftEffect $ Console.log familyStr
                    ]
                -}

                -- mbNextNode <-
                _ <- case (/\) <$> mbSelectedFamily <*> ((/\) <$> mbCurrentPatch <*> mbCurrentPatchId) of
                    Just (familyR /\ curPatch /\ curPatchId) ->
                        Hydra.withFamily
                            (NodeBox.fromFamily curPatchId curPatch)
                            familyR
                    Nothing -> pure Nothing
                -- liftEffect $ Console.log $ show selected
                {- -}

                {-
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
                            -- , Core.on ListBar.Select
                            --     \_ _ -> do
                            --         liftEffect $ Console.log "inlet"
                            --         inletSelected <- List.selected ~< nextInletsBar
                            --         liftEffect $ Console.log $ show inletSelected
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
                            -- , Core.on ListBar.Select
                            --     \_ _ -> do
                            --         liftEffect $ Console.log "outlet"
                            --         outletSelected <- List.selected ~< nextOutletsBar
                            --         liftEffect $ Console.log $ show outletSelected
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
                -}

                pure unit
        ]
        []
        \_ ->
            pure unit


toItem :: Id.FamilyR -> String
toItem familyR =
    let color = mark $ Hydra.toGroupR familyR
    in T.render $ T.fgc color $ T.s $ Id.reflectFamilyR familyR