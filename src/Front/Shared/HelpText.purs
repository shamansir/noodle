
module Front.Shared.HelpText where

import Prelude

import Data.Set (Set)
import Data.Array as Array
import Data.String as String
import Data.String.Extra2 as StringX
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (class Newtype, wrap, unwrap)

import Front.Shared.Keyboard as K


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


data SeqItem
    = SDesc String
    | SCombo K.Combo
    | SOpen


data HelpLine
    = Start
    | Continue String
    | Code String
    | Combo K.Combo
    | Sequence (Array SeqItem)


st :: HelpLine
st = Start
cont :: String -> HelpLine
cont = Continue
key :: K.Combo -> HelpLine
key = Combo
seq :: Array SeqItem -> HelpLine
seq = Sequence
code :: String -> HelpLine
code = Code


sd :: String -> SeqItem
sd = SDesc
skey :: K.Combo -> SeqItem
skey = SCombo
so :: SeqItem
so = SOpen


newtype HelpText = HelpText (Array HelpLine)

derive instance Newtype HelpText _


helpText :: Controller -> PossibleAction -> HelpText
helpText controller = wrap <<< case _ of

    {- General Interface -}

    GeneralInterface HideInterface ->
        [ st, cont "Hide the user interface using ", key K.space, cont " leaving only the canvas visible"
        , cont "(and get it back by pressing ", key K.space, cont " again)"
        ]
    GeneralInterface ToggleSolidBackground ->
        [ st, cont "Press ", key (K.w_shift $ K.space), cont " to toggle solid background"
        ]

    GeneralInterface ToggleTransparentBackground ->
        [ st, cont "Press ", key (K.w_shift $ K.space), cont " to toggle transparent background"
        ]

    GeneralInterface ShowInterface ->
        [ st, cont "Show the user interface using ", key K.space
        ]
    GeneralInterface StepBackInKeyboardCombo ->
        [ st, cont "Press ", key $ K.backspace, cont " to step back in the keyboard combo sequence"
        ]
    GeneralInterface ObserveKeyboardCombo ->
        [ st, cont "You can track the keyboard combo sequence in the status bar"
        ]

    {- Library -}

    Library SpawnByFamily ->
        case controller of
            Mouse ->
                [ st, cont "Spawn a node by clicking the desired family in the Library on the left"
                , cont "You can scroll the list using mouse wheel or trackpad"
                ]
            Keyboard ->
                [ st, cont "Use keyboard key ", key $ K.key "l", cont " to focus on the Library"
                ]
    Library SelectFamilyToSpawn ->
        [ st, cont "Press the index-key that is shown close to the family name and press ", key K.enter, cont " to confirm"
        ]
    Library ConfirmFamilyToSpawn ->
        [ st, cont "Press ", key K.enter, cont " to spawn the node of the selected family"
        ]

    {- CommandInput -}

    CommandInput LaunchCommandInput ->
        [ st, cont "Call command input by pressing ", key K.tab ]
    CommandInput EnterCommand ->
        [ st, cont "Try typing the node family and pressing ", key K.enter, cont ", for example ", code "osc", cont " will spawn the corresponding node"
        , st, cont "You can also create custom nodes by typing something like ", code ":: <a:Number -> b:Number -> c:Number> => <out>", cont " or ", code ":: <tuple> => <fst -> snd>", cont ", just start with double-semicolon"
        , st, cont "To close the input without applying, press ", key K.escape
        , st, cont "To apply the command, press ", key K.enter, cont " after entering it"
        ]

    {- Patches -}

    Patches CreatePatch ->
        -- [ st, cont "Create a patch by selecting multiple nodes (hold ", key K.shift, cont " and click on nodes), then press ", key $ K.key "p"
        -- ]
        [ st, cont "Create a new patch by clicking (+) button in the patches bar"
        ]
    Patches SelectPatch ->
        [ st, cont "Select a patch by clicking on its name in the patches bar"
        ]

    {- One node -}

    PatchArea (OneNode AddToSelection) ->
        case controller of
            Mouse ->
                [ st, cont "To select a node, hover over it and click on its body" ]
            Keyboard ->
                [ st, cont "Press ", key $ K.key "n", cont " to start selecting nodes using keyboard" ]
    PatchArea (OneNode SelectInletsOrOutlets) ->
        [ st, cont "Press ", key $ K.key "i", cont " to start selecting the particular inlet"
        , st, cont "Press ", key $ K.key "o", cont " to start selecting the particular outlet"
        ]
    PatchArea (OneNode SelectInlet) ->
        [ st, cont "Press the corresponding index-key that is shown close in the connector near the inlet name"
        ]
    PatchArea (OneNode SelectOutlet) ->
        [ st, cont "Press the corresponding index-key that is shown close in the connector near the outlet name"
        ]
    PatchArea (OneNode EditInletValue) ->
        [ st, cont "Changing value sends it to the inlet right away"
        ]
    PatchArea (OneNode FinishEditingInletValue) ->
        [ st, cont "To close the value editor, place ", key K.escape, cont " or ", key K.enter, cont ""
        ]

    {- Some nodes -}

    PatchArea (SomeNodes DragNodes) ->
        case controller of
            Mouse ->
                [ st, cont "To drag a node, hover over it and click the four-arrow button, then drag to the desired position and confirm by clicking again"
                ]
            Keyboard ->
                [ ]
    PatchArea (SomeNodes FinishDraggingNodes) ->
        [ st, cont "Drag the node to the desired position and confirm by clicking the four-arrow button again"
        ]
    PatchArea (SomeNodes MoveNodes) ->
        case controller of
            Mouse ->
                [ ]
            Keyboard ->
                [ st, cont "Move the node using arrow keys. To move faster, hold ", key K.shift, cont " while pressing the arrow keys"
                ]
    PatchArea (SomeNodes ConfirmMovingNodes) ->
        [ st, cont "Press ", key K.enter, cont " to confirm the position"
        ]
    PatchArea (SomeNodes CancelMovingNodes) ->
        [ st, cont "Press ", key K.escape, cont " to move the node back to its previous place"
        ]
    PatchArea (SomeNodes DeselectNodes) ->
        [ st, cont "To deselect a node, click on it"
        ]
    PatchArea (SomeNodes DeleteNodes) ->
        [ st, cont "To remove some node, hover over it and click the cross button"
        , st, cont "Alternatively, press ", key K.delete, cont " or ", key $ K.key "x"
        ]

    {- Patch area -}

    PatchArea SelectNodeToConnectFrom ->
        case controller of
            Mouse ->
                [ st, cont "To start connecting nodes, click on the desired outlet where the link should start"
                ]
            Keyboard ->
                [ st, cont "To connect nodes with keyboard, select the node to connect from using ", seq [ skey $ K.key "n", sd "node-index", so ]
                , st, cont "then select the outlet using ", seq [ so, skey $ K.key "o", sd "outlet-index", so ], cont ", and follow further instructions"
                ]
    PatchArea SelectOutletToConnectFrom ->
        case controller of
            Mouse ->
                [ st, cont "To start connecting nodes, click on the desired outlet where the link should start"
                ]
            Keyboard ->
                [ st, cont "Select the outlet using ", seq [ so, skey $ K.key "o", sd "outlet-index", so ], cont ", and follow further instructions"
                ]
    PatchArea SelectNodeToConnectTo ->
        case controller of
            Mouse ->
                [ st, cont "To finish creating a link, click on the inlet you want to connect to"
                ]
            Keyboard ->
                [ st, cont "select the node to connect to using ", seq [ so, sd "node-index", so ]
                ]
    PatchArea SelectInletToConnectTo ->
        case controller of
            Mouse ->
                [ st, cont "To finish creating a link, click on the inlet you want to connect to"
                ]
            Keyboard ->
                [ st, cont "select the inlet to connect to using ", seq [ so, skey $ K.key "i", sd "inlet-index" ]
                ]
    PatchArea ConfirmConnectingNodes ->
        [
        ]
    PatchArea CancelConnectingNodes ->
        [ st, cont "To cancel creating link, click somewhere on the empty area or press ", key K.escape
        ]
    PatchArea ChangeZoom ->
        [ st, cont "Zoom with Shift+MouseWheel or ", key $ K.w_ctrl K.plus, cont "/", key $ K.w_ctrl K.minus
        ]
    PatchArea ResetZoom ->
        [ st, cont "Reset zoom by clicking (*) in the status bar"
        ]
    PatchArea DisconnectLink ->
        [ st, cont "To disconnect a link, just click somewhere on its shape"
        ]
    PatchArea HoverForDocumentation ->
        [ st, cont "Hover over inlets, outlets, nodes, to get the information in status bar"
        ]


render :: HelpText -> String
render = String.joinWith " " <<< map _renderLine <<< unwrap


_renderLine :: HelpLine -> String
_renderLine = case _ of
    Start -> "" -- "* "
    Continue s -> StringX.escapeHtml s
    Code s -> "<code>" <> StringX.escapeHtml s <> "</code>"
    Combo c -> renderCombo c
    Sequence items -> "[" <> String.joinWith "-" (renderSeqItem <$> items) <> "]"
    where
    renderCombo :: K.Combo -> String
    renderCombo = case _ of
        K.Combo { key, mods, special } ->
            String.joinWith "+"
                $ Array.snoc (wrapKey <$> mods)
                $ if special then wrapSpecialKey key else wrapKey key
        where
            wrapKey k = "<kbd>" <> k <> "</kbd>"
            wrapSpecialKey k = "<kbd class=\"noodle-key-special\">" <> k <> "</kbd>"
    renderSeqItem :: SeqItem -> String
    renderSeqItem = case _ of
        SDesc s -> StringX.escapeHtml s
        SCombo c -> renderCombo c
        SOpen -> "..."