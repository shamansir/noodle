
module Front.Shared.HelpText where

import Prelude

import Data.Set (Set)
import Data.Tuple.Nested ((/\), type (/\))


data Controller
    = Mouse
    | Keyboard


derive instance Ord Controller
derive instance Eq Controller


data GeneralInterfaceAction
    = ShowInterface
    | HideInterface
    | ToggleSolidBackground
    | ToggleTransparentBackground
    | StepBackInKeyboardCombo
    | ObserveKeyboardCombo


data PatchesAction
    = CreatePatch
    | SelectPatch


data PatchAreaAction
    = DisconnectLink
    | SelectNodeToConnectFrom
    | SelectOutletToConnectFrom
    | SelectNodeToConnectTo
    | SelectInletToConnectTo
    | ConfirmConnectingNodes
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


derive instance Eq GeneralInterfaceAction
derive instance Ord GeneralInterfaceAction
derive instance Eq LibraryAction
derive instance Ord LibraryAction
derive instance Eq CommandInputAction
derive instance Ord CommandInputAction
derive instance Eq NodesAction
derive instance Ord NodesAction
derive instance Eq PatchesAction
derive instance Ord PatchesAction
derive instance Eq PatchAreaAction
derive instance Ord PatchAreaAction
derive instance Eq NodeAction
derive instance Ord NodeAction
derive instance Eq PossibleAction
derive instance Ord PossibleAction


class ProvidesHelp a where
    nextActions :: a -> Set (Controller /\ PossibleAction)


newtype Context = Context (Set (Controller /\ PossibleAction))


empty = Context mempty :: Context


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

    GeneralInterface ToggleTransparentBackground ->
        [ "Press <shift>+<space> to toggle transparent background"
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
                [ "To select a node, hover over it and click on its body" ]
            Keyboard ->
                [ "Press <n> to start selecting nodes using keyboard" ]
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
        [ "To deselect a node, click on it" -- FIXME: really?
        ]
    PatchArea (SomeNodes DeleteNodes) ->
        [ "To remove some node, hover over it and click the cross button"
        , "Alternatively, press <delete> or <x>"
        ]

    {- Patch area -}

    PatchArea SelectNodeToConnectFrom ->
        case controller of
            Mouse ->
                [ "To start connecting nodes, click on the desired outlet where the link should start"
                ]
            Keyboard ->
                [ "To connect nodes with keyboard, select the node to connect from using <n>-[node-index]-..."
                , "then select the outlet using ...-<o>-[outlet-index]-..., and follow further instructions"
                ]
    PatchArea SelectOutletToConnectFrom ->
        case controller of
            Mouse ->
                [ "To start connecting nodes, click on the desired outlet where the link should start"
                ]
            Keyboard ->
                [ "Select the outlet using ...-<o>-[outlet-index]-..., and follow further instructions"
                ]
    PatchArea SelectNodeToConnectTo ->
        case controller of
            Mouse ->
                [ "To finish creating a link, click on the inlet you want to connect to"
                ]
            Keyboard ->
                [ "select the node to connect to using ...-[node-index]-..."
                ]
    PatchArea SelectInletToConnectTo ->
        case controller of
            Mouse ->
                [ "To finish creating a link, click on the inlet you want to connect to"
                ]
            Keyboard ->
                [ "select the inlet to connect to using ...-<i>-[inlet-index]"
                ]
    PatchArea ConfirmConnectingNodes ->
        [
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


