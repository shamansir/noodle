module Web.Components.HelpText where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (intersperse) as Array

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (style) as HHP


data Context
    = Unknown
    | Start { zoomChanged :: Boolean, hasNodes :: Boolean, hasLinks :: Boolean }
    | InterfaceHidden
    | CommandInputOpen
    | CreatingLink
    | DraggingNode
    | EnteringValue


helpText :: Context -> Array String
helpText = case _ of
    Unknown ->
        []
    Start { zoomChanged, hasNodes, hasLinks } ->
        ( if not hasNodes then
            [ "Spawn a node by selecting its family in the Library list of families"
            , "You can scroll the list in the Library using mouse wheel or trackpad"
            , "Press space to hide interface and leave only canvas"
            ]
        else
            [ "Drag a node to any place by hovering over it and clicking the four-arrow button"
            , "To connect nodes, click the connector of an outlet you want to connect from on one node, release the mouse, and then click the inlet to where the data should flow"
            , if hasLinks then "To disconnect a link, just click somewhere on its shape" else ""
            , "To remove some node, hover over it and click the cross button"
            , "To edit value in the inlet, click on the value text"
            ]
        ) <>
        [ "Call command input by pressing tab"
        , "Hide the interface by pressing space"
        , if zoomChanged then "Zoom with Shift+MouseWheel, reset zoom by clicking (*) in the status bar" else "Zoom with Shift+MouseWheel"
        , "Hover over inlets, outlets, nodes, to get the information in status bar"
        ]
        <> alwaysWithUi
    InterfaceHidden ->
        [ "Press space to bring back the interface"
        , "Press ctrl+H to toggle this help text"
        ]
    CommandInputOpen ->
        [ "Try typing the node family and pressing Enter, for example `osc` will spawn the corresponding node"
        , "You can also create custom nodes by typing something like `:: <a:Number -> b:Number -> c:Number> => <out>` or `:: <tuple> => <fst -> snd>`, just start with double-semicolon"
        , "To close the input without applying, press Escape"
        , "To apply the command, press Enter after entering it"
        ]
        <> alwaysWithUi
    CreatingLink ->
        [ "To cancel creating link, click somewhere on the empty area or press Escape"
        ]
        <> alwaysWithUi
    EnteringValue ->
        [ "Changing value sends it to the inlet right away"
        , "To close the value editor, place Escape or Enter"
        ]
    DraggingNode ->
        [ "You can release the mouse button and drag anywhere, confirm the drag by clicking at the target position"
        ]
    where
        alwaysWithUi =
            [ "Press space to hide interface and leave only canvas"
            , "Press shift+space to toggle solid background"
            , "Press ctrl+H to toggle this help text"
            ]


type Input = Context


type State = Context


data Action
    = Receive Input


component
    :: forall query output m. H.Component query Input output m
component =
    H.mkComponent
        { initialState
        , render
        , eval : H.mkEval H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< Receive
            }
        }
    where
    initialState ctx = ctx

    maxWidth = 500.0

    render context =
        HH.div
            [ HHP.style $ "display: block; text-align: right; position: absolute; top: 0; right: 0; max-width: " <> show maxWidth <> "px;" ]
            $ Array.intersperse HH.br_ $ (HH.span [] <$> pure <$> HH.text <$> helpText context)

    handleAction = case _ of
        Receive context ->
            H.put context