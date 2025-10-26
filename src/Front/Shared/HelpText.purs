
module Front.Shared.HelpText where

import Data.Text.Format as T


import Prelude

import Data.Array (intersperse) as Array
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))


import Web.Components.SidePanel (SidePanel)


data Controller
    = Mouse
    | Keyboard


data GeneralInterfaceAction
    = ShowInterface
    | HideInterface
    | ToggleSolidBackground
    | StepBackInKeyboardCombo
    | ObserveKeyboardCombo


data PatchesAction
    = CreatePatch
    | SelectPatch


data PatchAreaAction
    = DisconnectLink
    | StartConnectingNodes
    | FinishConnectingNodes
    | CancelConnectingNodes
    | HoverForDocumentation
    | ChangeZoom
    | ResetZoom
    | OneNode NodeAction
    | SomeNodes NodesAction


data LibraryAction
    = SpawnByFamily
    | SelectFamilyToSpawn
    | ConfirmFamilyToSpawn


data CommandInputAction
    = LaunchCommandInput
    | EnterCommand


data NodeAction
    = AddToSelection
    | SelectInletsOrOutlets
    | SelectInlet
    | SelectOutlet
    | EditInletValue
    | FinishEditingInletValue


data NodesAction
    = DragNodes
    | MoveNodes
    | ConfirmMovingNodes
    | CancelMovingNodes
    | FinishDraggingNodes
    | DeleteNodes
    | DeselectNodes


data PossibleAction
    = GeneralInterface GeneralInterfaceAction
    | Patches PatchesAction
    | PatchArea PatchAreaAction
    | Library LibraryAction
    | CommandInput CommandInputAction
    -- | Links
    -- | ValueEditor


class ProvidesHelp a where
    nextActions :: a -> Array (Controller /\ PossibleAction)


newtype Context = Context (Array (Controller /\ PossibleAction))


empty = Context [] :: Context


both :: PossibleAction -> Array (Controller /\ PossibleAction)
both act = [ Mouse /\ act, Keyboard /\ act ]


helpText :: Controller -> PossibleAction -> Array String
helpText controller = case _ of

    {- General Interface -}

    GeneralInterface HideInterface ->
        [ "Hide the user interface using <space>, leaving only the canvas visible"
        , "(and get it back by pressing <space> again)"
        ]
    GeneralInterface ToggleSolidBackground ->
        [ "Press <shift>+<space> to toggle solid background"
        ]
    GeneralInterface ShowInterface ->
        [ "Show the user interface using <space>"
        ]
    GeneralInterface StepBackInKeyboardCombo ->
        [ "Press <backspace> to step back in the keyboard combo sequence"
        ]
    GeneralInterface ObserveKeyboardCombo ->
        [ "You can track the keyboard combo sequence in the status bar"
        ]


    {- Library -}

    Library SpawnByFamily ->
        case controller of
            Mouse ->
                [ "Spawn a node by clicking the desired family in the Library on the left"
                , "You can scroll the list using mouse wheel or trackpad"
                ]
            Keyboard ->
                [ "Use keyboard key <l> to focus on the Library"
                ]
    Library SelectFamilyToSpawn ->
        [ "Press the index-key that is shown close to the family name and press <enter> to confirm"
        ]
    Library ConfirmFamilyToSpawn ->
        [ "Press <enter> to spawn the node of the selected family"
        ]

    {- CommandInput -}

    CommandInput LaunchCommandInput ->
        [ "Call command input by pressing <tab>" ]
    CommandInput EnterCommand ->
        [ "Try typing the node family and pressing <enter>, for example `osc` will spawn the corresponding node"
        , "You can also create custom nodes by typing something like `:: <a:Number -> b:Number -> c:Number> => <out>` or `:: <tuple> => <fst -> snd>`, just start with double-semicolon"
        , "To close the input without applying, press <escape>"
        , "To apply the command, press <enter> after entering it"
        ]

    {- Patches -}

    Patches CreatePatch ->
        -- [ "Create a patch by selecting multiple nodes (hold <shift> and click on nodes), then press <p>"
        -- ]
        [ "Create a new patch by clicking (+) button in the patches bar"
        ]
    Patches SelectPatch ->
        [ "Select a patch by clicking on its name in the patches bar"
        ]

    {- One node -}

    PatchArea (OneNode AddToSelection) ->
        case controller of
            Mouse ->
                [ "To select a node, hover over it and click on its body"
                ]
            Keyboard ->
                [ "Press <n> to start selecting nodes"
                ]
    PatchArea (OneNode SelectInletsOrOutlets) ->
        [ "Press <i> to start selecting the particular inlet"
        , "Press <o> to start selecting the particular outlet"
        ]
    PatchArea (OneNode SelectInlet) ->
        [ "Press the corresponding index-key that is shown close in the connector near the inlet name"
        ]
    PatchArea (OneNode SelectOutlet) ->
        [ "Press the corresponding index-key that is shown close in the connector near the outlet name"
        ]
    PatchArea (OneNode EditInletValue) ->
        [ "Changing value sends it to the inlet right away"
        ]
    PatchArea (OneNode FinishEditingInletValue) ->
        [ "To close the value editor, place <escape> or <enter>"
        ]

    {- Some nodes -}

    PatchArea (SomeNodes DragNodes) ->
        case controller of
            Mouse ->
                [ "To drag a node, hover over it and click the four-arrow button, then drag to the desired position and confirm by clicking again"
                ]
            Keyboard ->
                [ ]
    PatchArea (SomeNodes FinishDraggingNodes) ->
        [ "Drag the node to the desired position and confirm by clicking the four-arrow button again"
        ]
    PatchArea (SomeNodes MoveNodes) ->
        case controller of
            Mouse ->
                [ ]
            Keyboard ->
                [ "Move the node using arrow keys. To move faster, hold <shift> while pressing the arrow keys"
                ]
    PatchArea (SomeNodes ConfirmMovingNodes) ->
        [ "Press <enter> to confirm the position"
        ]
    PatchArea (SomeNodes CancelMovingNodes) ->
        [ "Press <escape> to move the node back to its previous place"
        ]
    PatchArea (SomeNodes DeselectNodes) ->
        []
    PatchArea (SomeNodes DeleteNodes) ->
        [ "To remove some node, hover over it and click the cross button"
        , "Alternatively, press <delete> or <x>"
        ]

    {- Patch area -}

    PatchArea StartConnectingNodes ->
        []
    PatchArea FinishConnectingNodes ->
        [ "To finish creating link, click on the inlet you want to connect to"
        ]
    PatchArea CancelConnectingNodes ->
        [ "To cancel creating link, click somewhere on the empty area or press <escape>"
        ]
    PatchArea ChangeZoom ->
        [ "Zoom with Shift+MouseWheel or <ctrl>+<+>/<->"
        ]
    PatchArea ResetZoom ->
        [ "Reset zoom by clicking (*) in the status bar"
        ]
    PatchArea DisconnectLink ->
        [ "To disconnect a link, just click somewhere on its shape"
        ]
    PatchArea HoverForDocumentation ->
        [ "Hover over inlets, outlets, nodes, to get the information in status bar"
        ]


