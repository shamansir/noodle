
module Front.Shared.HelpText where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Set (Set)
import Data.String as String
import Data.String.Extra2 as StringX
import Data.Text.Format (u)
import Data.Tuple (curry, uncurry) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Front.Shared.Keyboard as K
import Noodle.Fn.Signature (o)


-- data Controller
--     = Mouse
--     | Keyboard


-- derive instance Ord Controller
-- derive instance Eq Controller


data GeneralInterfaceAction
    = KB_ShowInterface
    | KB_HideInterface
    | KB_ToggleSolidBackground
    | KB_ToggleTransparentBackground
    | KB_StepBackInKeyboardCombo
    -- | M_ToggleInterfaceMode
    | G_ObserveKeyboardCombo


data PatchesAction
    = M_CreatePatch
    | M_SelectPatch
    | KB_PatchesBar
    | KB_CreatePatch
    | KB_SelectPatch


data PatchAreaAction
    = M_DisconnectLink
    | KB_Links
    | KB_DisconnectLink
    | M_SelectOutletToStartLink
    | M_SelectInletToFinishLink
    | KB_SelectNodeToConnectFrom
    | KB_SelectOutletToConnectFrom
    | KB_SelectNodeToConnectTo
    | KB_SelectInletToConnectTo
    | KB_ConfirmConnectingNodes
    | M_CancelConnectingNodes
    | KB_CancelConnectingNodes
    | M_HoverForDocumentation
    | KB_ChangeZoom
    | M_ChangeZoom
    | KB_ResetZoom
    | M_ResetZoom
    | G_OneNode NodeAction
    | G_SomeNodes NodesAction


data LibraryAction
    = M_SpawnByFamily
    | KB_FocusOnLibrary
    | KB_SelectFamilyToSpawn
    | KB_ConfirmFamilyToSpawn


data CommandInputAction
    = KB_LaunchCommandInput
    | KB_EnterCommand


data NodeAction
    = M_AddToSelection
    | KB_AddToSelection
    | KB_SelectInletsSide
    | KB_SelectOutletsSide
    | KB_SelectInlet
    | KB_SelectOutlet
    | M_SpawnValueEditor -- ?? Call editro
    | KB_EditInletValue
    | M_FinishEditingInletValue
    | KB_FinishEditingInletValue


data NodesAction
    = KB_MoveNodes
    | M_DragNodes
    | KB_ConfirmMovingNodes
    | KB_CancelMovingNodes
    | M_FinishDraggingNodes
    | KB_DeleteNodes
    | M_DeleteNodes
    | KB_DeselectNodes
    | M_DeselectNodes


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
    nextActions :: a -> Set PossibleAction


newtype Context = Context (Set PossibleAction)


empty = Context mempty :: Context


data MButton
    = MLeft
    | MMiddle
    | MRight
    | MWheel
    | MHover


data IndexFor
    = Node
    | Inlet
    | Outlet
    | Family
    | Patch


data SeqItem
    = SDesc String
    | SKeys K.Combo
    | SMouse MButton String
    | SIndexKey IndexFor
    | SOpen


data HelpLine
    = Start
    | Continue String
    | Code String
    | Sequence (NonEmptyArray SeqItem)
    | None


st :: HelpLine
st = Start
cont :: String -> HelpLine
cont = Continue
key :: K.Combo -> HelpLine
key = seq <<< pure <<< SKeys
seq :: Array SeqItem -> HelpLine
seq = maybe None Sequence <<< NEA.fromArray
code :: String -> HelpLine
code = Code
mclick :: String -> HelpLine
mclick = seq <<< pure <<< SMouse MLeft
mhover :: String -> HelpLine
mhover = seq <<< pure <<< SMouse MHover
mwheel :: String -> HelpLine
mwheel = seq <<< pure <<< SMouse MWheel
hindex :: IndexFor ->HelpLine
hindex = seq <<< pure <<< SIndexKey


sd :: String -> SeqItem
sd = SDesc
skey :: K.Combo -> SeqItem
skey = SKeys
so :: SeqItem
so = SOpen
sindex :: IndexFor -> SeqItem
sindex = SIndexKey
smwheel :: String -> SeqItem
smwheel = SMouse MWheel


ht :: Int -> Array HelpLine -> HelpText
ht p ls = HelpText { priority: p, content: ls }


newtype HelpText = HelpText { priority :: Int, content :: Array HelpLine }


derive instance Newtype HelpText _


helpText :: PossibleAction -> HelpText
helpText = Tuple.uncurry ht <<< case _ of

    {- General Interface -}

    GeneralInterface KB_HideInterface ->
        -11 /\
        [ st, cont "Hide the user interface using ", key K.space, cont " leaving only the canvas visible"
        , cont " (and get it back by pressing ", key K.space, cont " again)"
        ]
    GeneralInterface KB_ToggleSolidBackground ->
        -11 /\
        [ st, cont "Press ", key (K.w_shift $ K.space), cont " to toggle solid background"
        ]

    GeneralInterface KB_ToggleTransparentBackground ->
        -11 /\
        [ st, cont "Press ", key (K.w_shift $ K.space), cont " to toggle transparent background"
        ]

    GeneralInterface KB_ShowInterface ->
        -11 /\
        [ st, cont "Show the user interface using ", key K.space
        ]
    GeneralInterface KB_StepBackInKeyboardCombo ->
        0 /\
        [ st, cont "Press ", key $ K.backspace, cont " to step back in the keyboard combo sequence"
        ]
    GeneralInterface G_ObserveKeyboardCombo ->
        -10 /\
        [ st, cont "You can track the keyboard combo sequence in the status bar"
        ]

    {- Library -}

    Library M_SpawnByFamily ->
        -5 /\
        [ st, cont "Spawn a node by ", mclick "clicking", cont " the desired family in the Library on the left"
        , cont "You can scroll the list using ", mwheel "mouse wheel", cont " or trackpad"
        ]
    Library KB_FocusOnLibrary ->
        -5 /\
        [ st, cont "Use keyboard key ", key $ K.key "l", cont " to focus on the Library"
        ]
    Library KB_SelectFamilyToSpawn ->
         1 /\
        [ st, cont "Press the ", hindex Family, cont " that is shown close to the family name and press ", key K.enter, cont " to confirm"
        ]
    Library KB_ConfirmFamilyToSpawn ->
         1 /\
        [ st, cont "Press ", key K.enter, cont " to spawn the node of the selected family"
        ]

    {- CommandInput -}

    CommandInput KB_LaunchCommandInput ->
        -5 /\
        [ st, cont "Call command input by pressing ", key K.tab ]
    CommandInput KB_EnterCommand ->
         3 /\
        [ st, cont "Try typing the node family and pressing ", key K.enter, cont ", for example ", code "osc", cont " will spawn the corresponding node"
        , st, cont "You can also create custom nodes by typing something like ", code ":: <a:Number -> b:Number -> c:Number> => <out>", cont " or ", code ":: <tuple> => <fst -> snd>", cont ", just start with double-semicolon"
        , st, cont "To close the input without applying, press ", key K.escape
        , st, cont "To apply the command, press ", key K.enter, cont " after entering it"
        ]

    {- Patches -}

    Patches M_CreatePatch ->
        -- [ st, cont "Create a patch by selecting multiple nodes (hold ", key K.shift, cont " and click on nodes), then press ", key $ K.key "p"
        -- ]
        -8 /\
        [ st, cont "Create a new patch by ", mclick "clicking", cont " (+) button in the patches bar"
        ]
    Patches M_SelectPatch ->
        -8 /\
        [ st, cont "Select a patch by ", mclick "clicking", cont " on its name in the patches bar"
        ]

    {- One node -}

    PatchArea (G_OneNode M_AddToSelection) ->
        -5 /\
        [ st, cont "To select a node, ", mhover "hover", cont " over it and ", mclick "click", cont " on its body" ]
    PatchArea (G_OneNode KB_AddToSelection) ->
        2 /\
        [ st, cont "Press ", key $ K.key "n", cont " to start selecting nodes using keyboard" ]
    PatchArea (G_OneNode KB_SelectInletsSide) ->
        5 /\
        [ st, cont "Press ", key $ K.key "i", cont " to start selecting the particular inlet."
        ]
    PatchArea (G_OneNode KB_SelectOutletsSide) ->
        5 /\
        [ st, cont "Press ", key $ K.key "o", cont " to start selecting the particular outlet"
        ]
    PatchArea (G_OneNode KB_SelectInlet) ->
        5 /\
        [ st, cont "Press the corresponding ", hindex Inlet, cont " that is shown close in the connector near the inlet name"
        ]
    PatchArea (G_OneNode KB_SelectOutlet) ->
        5 /\
        [ st, cont "Press the corresponding ", hindex Outlet, cont " that is shown close in the connector near the outlet name"
        ]
    PatchArea (G_OneNode M_SpawnValueEditor) ->
        -5 /\
        [ st, cont "To change the value at the inlet, ", mclick "click", cont " on it and the value editor will open"
        ]
    PatchArea (G_OneNode KB_EditInletValue) ->
        3 /\
        [ st, cont "To edit the inlet value, press " , key K.enter, cont " and type the desired value"
        ]
    PatchArea (G_OneNode KB_FinishEditingInletValue) ->
        6 /\
        [ st, cont "To close the value editor, place ", key K.escape, cont " or ", key K.enter
        ]
    PatchArea (G_OneNode M_FinishEditingInletValue) ->
        6 /\
        [ st, cont "To close the value editor, place ", key K.escape, cont " or ", key K.enter
        ]
    Patches KB_PatchesBar ->
        -20 /\
        [ ]
    Patches KB_CreatePatch ->
        -20 /\
        [ ]
    Patches KB_SelectPatch ->
        -20 /\
        [ ]

    {- Some nodes -}

    PatchArea (G_SomeNodes M_DragNodes) ->
        -10 /\
        [ st, cont "To drag a node, hover over it and ", mclick "click", cont " the four-arrow button, then drag to the desired position and confirm by ", mclick "clicking", cont " again"
        ]
    PatchArea (G_SomeNodes M_FinishDraggingNodes) ->
        0 /\
        [ st, cont "Drag the node to the desired position and confirm by ", mclick "clicking", cont " the four-arrow button again"
        ]
    PatchArea (G_SomeNodes KB_MoveNodes) ->
        5 /\
        [ st, cont "Move the node using ", key K.arrows, cont ". To move faster, hold ", key K.shift, cont " while pressing the  ", key K.arrows
        ]
    PatchArea (G_SomeNodes KB_ConfirmMovingNodes) ->
        5 /\
        [ st, cont "Press ", key K.enter, cont " to confirm the position"
        ]
    PatchArea (G_SomeNodes KB_CancelMovingNodes) ->
        -1 /\
        [ st, cont "Press ", key K.escape, cont " to move the node back to its previous place"
        ]
    PatchArea (G_SomeNodes M_DeselectNodes) ->
        0 /\
        [ st, cont "To deselect a node, ", mclick "click", cont " on it"
        ]
    PatchArea (G_SomeNodes M_DeleteNodes) ->
        0 /\
        [ st, cont "To remove some node, ", mhover "hover", cont " over it and ", mclick "click", cont " the cross button." ]
    PatchArea (G_SomeNodes KB_DeleteNodes) ->
        0 /\
        [ st, cont "To remove some node using keyboard, press ", key K.delete, cont " or ", key $ K.key "x"
        ]
    PatchArea (G_SomeNodes KB_DeselectNodes) ->
        -10 /\
        [ st, cont "To drag a node, hover over it and ", mclick "click", cont " the four-arrow button, then drag to the desired position and confirm by ", mclick "clicking", cont " again"
        ]

    {- Patch area -}

    PatchArea M_SelectOutletToStartLink ->
        -1 /\
        [ st, cont "To start connecting nodes, ", mclick "click", cont " on the desired outlet where the link should start"
        ]
    PatchArea M_SelectInletToFinishLink ->
        4 /\
        [ st, cont "To finish creating a link, ", mclick "click", cont " on the inlet you want to connect to"
        ]
    PatchArea KB_SelectNodeToConnectFrom ->
        -3 /\
        [ st, cont "To connect nodes with keyboard, select the node to connect from using ", seq [ skey $ K.key "n", sindex Node, so ]
        , st, cont "then select the outlet using ", seq [ so, skey $ K.key "o", sindex Outlet, so ], cont ", and follow further instructions"
        ]
    PatchArea KB_SelectOutletToConnectFrom ->
        5 /\
        [ st, cont "Select the outlet using ", seq [ so, skey $ K.key "o", sindex Outlet, so ], cont ", and follow further instructions"
        ]
    PatchArea KB_SelectNodeToConnectTo ->
        6 /\
        [ st, cont "select the node to connect to using ", seq [ so, sindex Node, so ]
        ]
    PatchArea KB_SelectInletToConnectTo ->
        6 /\
        [ st, cont "select the inlet to connect to using ", seq [ so, skey $ K.key "i", sindex Inlet ]
        ]
    PatchArea KB_ConfirmConnectingNodes ->
        -20 /\
        [ ]
    PatchArea M_CancelConnectingNodes ->
        4 /\
        [ st, cont "To cancel creating link, ", mclick "click", cont " somewhere on the empty area or press ", key K.escape
        ]
    PatchArea KB_CancelConnectingNodes ->
        4 /\
        [ st, cont "To cancel creating link press ", key K.escape
        ]
    PatchArea M_ChangeZoom ->
        -5 /\
        [ st, cont "Zoom with ", seq [ skey K.shift, smwheel "mouse wheel" ], cont " or ", key $ K.w_ctrl K.plus, cont "/", key $ K.w_ctrl K.minus
        ]
    PatchArea KB_ChangeZoom ->
        -5 /\
        [ st, cont "Zoom with ", seq [ skey K.shift, smwheel "mouse wheel" ], cont " or ", key $ K.w_ctrl K.plus, cont "/", key $ K.w_ctrl K.minus
        ]
    PatchArea M_ResetZoom ->
        -7 /\
        [ st, cont "Reset zoom by ", mclick "clicking", cont " (*) in the status bar"
        ]
    PatchArea KB_ResetZoom ->
        -20 /\ -- FIXME
        [ ]
    PatchArea M_DisconnectLink ->
        -1 /\
        [ st, cont "To disconnect a link, just ", mclick "click", cont " somewhere on its shape"
        ]
    PatchArea M_HoverForDocumentation ->
        -4 /\
        [ st, mhover "Hover", cont " over inlets, outlets, nodes, to get the information in status bar"
        ]

    PatchArea KB_Links ->
        -20 /\
        [ ]
    PatchArea KB_DisconnectLink ->
        -20 /\
        [ ]



render :: HelpText -> String
render = String.joinWith "" <<< map _renderLine <<< _.content <<< unwrap


isEmpty :: HelpText -> Boolean
isEmpty = unwrap >>> _.content >>> Array.null


renderAll :: Array HelpText -> Array String
renderAll =
    Array.filter (not isEmpty)
    >>> Array.sortWith (unwrap >>> _.priority)
    >>> Array.reverse
    >>> map render
    >>> Array.nub


_renderLine :: HelpLine -> String
_renderLine = case _ of
    Start -> "" -- "* "
    Continue s -> StringX.escapeHtml s
    Code s -> "<code>" <> StringX.escapeHtml s <> "</code>"
    Sequence items ->
        case NEA.uncons items of
            { head, tail } ->
                if Array.null tail then
                    renderSeqItem head
                else
                    "<span class=\"noodle-help-key-combo\">" <> String.joinWith "-" (NEA.toArray $ renderSeqItem <$> items) <> "</span>"
    None -> "---"
        -- FIXME: Handle empty sequence case
    where
    renderCombo :: K.Combo -> String
    renderCombo = case _ of
        K.Combo { key, mods, special } ->
            String.joinWith "+"
                $ Array.snoc (wrapSpecialKey <$> mods)
                $ if special then wrapSpecialKey key else wrapKey key
        where
            wrapKey k = "<kbd class=\"noodle-help-key\">" <> k <> "</kbd>"
            wrapSpecialKey k = "<kbd class=\"noodle-help-key-special\">" <> k <> "</kbd>"
    mouseWrap :: String -> String -> String -> String
    mouseWrap cls alt comment =
        "<span class=\"noodle-help-mouse noodle-help-" <> cls <> "\" alt=\"" <> alt <> "\">" <> StringX.escapeHtml comment <> "</span>"
    renderSeqItem :: SeqItem -> String
    renderSeqItem = case _ of
        SDesc s -> StringX.escapeHtml s
        SKeys c -> renderCombo c
        SOpen -> "..."
        SIndexKey for_ ->
            "<span class=\"noodle-help-index-key\">" <>
            ( case for_ of
                Node -> "node"
                Inlet -> "inlet"
                Outlet -> "outlet"
                Family -> "family"
                Patch -> "patch"
            ) <> " index key" <> "</span>"
        SMouse mb comment -> case mb of
            MLeft -> mouseWrap "left-click" "left click" comment
            MMiddle -> mouseWrap "middle-click" "middle click" comment
            MRight -> mouseWrap "right-click" "right click" comment
            MWheel -> mouseWrap "wheel" "mouse wheel" comment
            MHover -> mouseWrap "hover" "hover" comment