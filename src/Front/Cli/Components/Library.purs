module Cli.Components.Library where


import Control.Monad.State as State

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\))
import Data.Array ((!!))

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
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Lists.List.Event (ListEvent(..)) as List
import Blessed.UI.Lists.List.Option (items, keys, mouse, style) as List
import Blessed.UI.Lists.List.Property (selected) as List


import Noodle.Id as Id
import Noodle.Network as Network

import Cli.Keys as Key
import Cli.State (State)
import Cli.Style (library, libraryBorder) as Style

import Noodle.Ui.Cli.Palette as Palette
import Noodle.Ui.Cli.Tagging as T
import Noodle.Ui.Cli.Palette.Mark
import Noodle.Toolkit (class MarkToolkit)

-- import Cli.Components.NodeBox as NodeBox

import Data.Text.Format (fgc, s) as T
import Data.Text.Output.Blessed (singleLine) as T



import Prelude


component :: forall tk p fs r m. MarkToolkit tk => Array Id.FamilyR -> Core.Blessed (State tk p fs r m)
component families =
    B.listAnd Key.library
        [ Box.top $ Offset.px 0
        , Box.left $ Offset.px 0
        , Box.width $ Dimension.px 20
        , Box.height $ Dimension.percents 65.0
        , Box.draggable true
        , Box.scrollable true
        , List.items $ (T.singleLine <<< T.libraryItem (Proxy :: _ tk)) <$> families
        , List.mouse true
        , List.keys true
        , Box.tags true
        , Style.library
        , Style.libraryBorder
        -- , Core.on Element.MouseOver
        --     \_ _ -> do
        --         selected <- List.selected ~< Key.library
        --         liftEffect $ Console.log $ show selected
        , Core.on List.Select
            \_ _ -> do
                -- lastShiftX <- _.lastShiftX <$> State.get
                -- lastShiftY <- _.lastShiftY <$> State.get
                -- lastNodeBoxKey <- _.lastNodeBoxKey <$> State.get
                state <- State.get

                {- -}
                let mbCurrentPatchId = _.id <$> state.currentPatch
                let mbCurrentPatch = mbCurrentPatchId >>= \id -> Network.patch id state.network
                {- -}
                -- patchesBar >~ ListBar.addItemH ?wh [] ?wh

                -- Hydra.withFamily

                -- let top = Offset.px $ state.lastShiftX + 2
                -- let left = Offset.px $ 16 + state.lastShiftY + 2
                -- let nextNodeBox = NodeKey.next state.lastNodeBoxKey
                -- let nextInputsBox = NodeKey.next state.lastInputsBoxKey
                -- let nextOutputsBox = NodeKey.next state.lastOutputsBoxKey

                {- -}
                selected <- List.selected ~< Key.library
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
                        pure Nothing
                        {- Hydra.withFamily
                            (NodeBox.fromFamilyAuto curPatchId curPatch)
                            familyR -}
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
                                        [ Border.fg palette.librarySelection
                                        ]
                                    ]
                                ]
                            , Core.on Element.Move $ onNodeMove nextNodeBox -- FIXME: onNodeMove receives wrong `NodeKey` in the handler, probably thanks to `proxies` passed around
                            ]
                            [ ]

                let
                    inputHandler idx iname =
                        iname /\ [] /\ onInputSelect nextNodeBox idx iname
                    inputsBoxN =
                        B.listbar nextInputsBox
                            [ Box.width $ Dimension.percents 90.0
                            , Box.height $ Dimension.px 1
                            , Box.top $ Offset.px 0
                            , Box.left $ Offset.px 0
                            -- , List.items is
                            , ListBar.commands $ mapWithIndex inputHandler is
                            , List.mouse true
                            , List.keys true
                            , ListBar.autoCommandKeys true
                            , inputsOutputsStyle
                            -- , Core.on ListBar.Select
                            --     \_ _ -> do
                            --         liftEffect $ Console.log "input"
                            --         inputSelected <- List.selected ~< nextInputsBox
                            --         liftEffect $ Console.log $ show inputSelected
                            ]
                            [ ]


                let
                    outputHandler idx oname =
                        oname /\ [] /\ onOutputSelect nextNodeBox idx oname
                    outputsBoxN =
                        B.listbar nextOutputsBox
                            [ Box.width $ Dimension.percents 90.0
                            , Box.height $ Dimension.px 1
                            , Box.top $ Offset.px 2
                            , Box.left $ Offset.px 0
                            -- , List.items os
                            , ListBar.commands $  mapWithIndex outputHandler os
                            , List.mouse true
                            , List.keys true
                            , inputsOutputsStyle
                            -- , Core.on ListBar.Select
                            --     \_ _ -> do
                            --         liftEffect $ Console.log "output"
                            --         outputSelected <- List.selected ~< nextOutputsBox
                            --         liftEffect $ Console.log $ show outputSelected
                            ]
                            [
                            ]

                patchBox >~ Node.append nextNodeBoxN
                nextNodeBox >~ Node.append inputsBoxN
                nextNodeBox >~ Node.append outputsBoxN

                State.modify_ (_
                    { lastShiftX = state.lastShiftX + 1
                    , lastShiftY = state.lastShiftY + 1
                    , lastNodeBoxKey = nextNodeBox
                    , lastInputsBoxKey = nextInputsBox
                    , lastOutputsBoxKey = nextOutputsBox
                    } )

                mainScreen >~ Screen.render
                -}

                pure unit
        ]
        []
        \_ ->
            pure unit
