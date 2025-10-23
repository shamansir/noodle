module Web.Components.HelpText where

import Prelude

import Blessed.UI.Forms.Form.Event (FormEvent(..))
import Data.Array (intersperse) as Array
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (style) as HHP
import Web.Components.AppScreen.KeyboardLogic (Action(..)) as KL


data Context
    = Unknown
    | Start { zoomChanged :: Boolean, hasNodes :: Boolean, hasLinks :: Boolean }
    | InterfaceHidden
    | CommandInputOpen
    | CreatingLink
    | DraggingNode
    | EnteringValue


data Controller
    = Mouse
    | Keyboard


data PossibleAction
    = HideInterface
    | ToggleSolidBackground
    | ShowInterface
    | LaunchCommandInput
    | EnterCommand
    | SpawnFromLibrary
    | SelectFamilyToSpawn
    | CreatePatch
    | SelectPatch
    | DragNode
    | MoveNode
    | ConfirmMovingNode
    | CancelMovingNode
    | FinishDraggingNode
    | SelectNode
    | SelectInletsOrOutlets
    | SelectInlet
    | SelectOutlet
    | EditInletValue
    | FinishEditingInletValue
    | StartConnectingNodes
    | FinishConnectingNodes
    | CancelConnectingNodes
    | RemoveLinks
    | SelectMoreNodes
    | DeselectNodes
    | DeleteNode
    | ChangeZoom
    | ResetZoom
    | HoverForDocumentation
    | StepBackInKeyboardCombo
    | ObserveKeyboardCombo


helpText' :: Controller -> PossibleAction -> Array String
helpText' controller = case _ of
    HideInterface ->
        [ "Hide the user interface using <space>, leaving only the canvas visible"
        , "(and get it back by pressing <space> again)"
        ]
    ToggleSolidBackground ->
        [ "Press <shift>+<space> to toggle solid background"
        ]
    ShowInterface ->
        [ "Show the user interface using <space>"
        ]
    SpawnFromLibrary ->
        case controller of
            Mouse ->
                [ "Spawn a node by clicking the desired family in the Library on the left"
                , "You can scroll the list using mouse wheel or trackpad"
                ]
            Keyboard ->
                [ "Use keyboard key <l> to focus on the Library"
                ]
    SelectFamilyToSpawn ->
        [ "Press the index-key that is shown close to the family name and press <enter> to confirm"
        ]
    LaunchCommandInput ->
        [ "Call command input by pressing <tab>" ]
    EnterCommand ->
        [ "Try typing the node family and pressing <enter>, for example `osc` will spawn the corresponding node"
        , "You can also create custom nodes by typing something like `:: <a:Number -> b:Number -> c:Number> => <out>` or `:: <tuple> => <fst -> snd>`, just start with double-semicolon"
        , "To close the input without applying, press <escape>"
        , "To apply the command, press <enter> after entering it"
        ]
    CreatePatch ->
        -- [ "Create a patch by selecting multiple nodes (hold <shift> and click on nodes), then press <p>"
        -- ]
        [ "Create a new patch by clicking (+) button in the patches bar"
        ]
    SelectPatch ->
        [ "Select a patch by clicking on its name in the patches bar"
        ]
    DragNode ->
        case controller of
            Mouse ->
                [ "To drag a node, hover over it and click the four-arrow button, then drag to the desired position and confirm by clicking again"
                ]
            Keyboard ->
                [ ]
    FinishDraggingNode ->
        [ "Drag the node to the desired position and confirm by clicking the four-arrow button again"
        ]
    MoveNode ->
        case controller of
            Mouse ->
                [ ]
            Keyboard ->
                [ "Move the node using arrow keys. To move faster, hold <shift> while pressing the arrow keys"
                ]
    ConfirmMovingNode ->
        [ "Press <enter> to confirm the position"
        ]
    CancelMovingNode ->
        [ "Press <escape> to move the node back to its previous place"
        ]
    SelectNode ->
        case controller of
            Mouse ->
                [ "To select a node, hover over it and click on its body"
                ]
            Keyboard ->
                [ "Press <n> to start selecting nodes"
                ]
    SelectInletsOrOutlets ->
        [ "Press <i> to start selecting the particular inlet"
        , "Press <o> to start selecting the particular outlet"
        ]
    SelectInlet ->
        [ "Press the corresponding index-key that is shown close in the connector near the inlet name"
        ]
    SelectOutlet ->
        [ "Press the corresponding index-key that is shown close in the connector near the outlet name"
        ]
    EditInletValue ->
        [ "Changing value sends it to the inlet right away"
        ]
    FinishEditingInletValue ->
        [ "To close the value editor, place <escape> or <enter>"
        ]
    StartConnectingNodes ->
        []
    FinishConnectingNodes ->
        [ "To finish creating link, click on the inlet you want to connect to"
        ]
    CancelConnectingNodes ->
        [ "To cancel creating link, click somewhere on the empty area or press <escape>"
        ]
    SelectMoreNodes ->
        []
    DeselectNodes ->
        []
    DeleteNode ->
        [ "To remove some node, hover over it and click the cross button"
        , "Alternatively, press <delete> or <x>"
        ]
    ChangeZoom ->
        [ "Zoom with Shift+MouseWheel or <ctrl>+<+>/<->"
        ]
    ResetZoom ->
        [ "Reset zoom by clicking (*) in the status bar"
        ]
    RemoveLinks ->
        [ "To disconnect a link, just click somewhere on its shape"
        ]
    HoverForDocumentation ->
        [ "Hover over inlets, outlets, nodes, to get the information in status bar"
        ]
    StepBackInKeyboardCombo ->
        [ "Press <backspace> to step back in the keyboard combo sequence"
        ]
    ObserveKeyboardCombo ->
        [ "You can track the keyboard combo sequence in the status bar"
        ]



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
            [ HHP.style $ "display: block; text-align: right; position: absolute; top: 50px; right: 370px; max-width: " <> show maxWidth <> "px;" ]
            $ Array.intersperse HH.br_ $ (HH.span [] <$> pure <$> HH.text <$> helpText context)

    handleAction = case _ of
        Receive context ->
            H.put context