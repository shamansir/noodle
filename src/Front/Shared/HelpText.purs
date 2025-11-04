
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


helpText :: Controller -> PossibleAction -> HelpText
helpText controller = Tuple.uncurry ht <<< case _ of

    {- General Interface -}

    GeneralInterface HideInterface ->
        -11 /\
        [ st, cont "Hide the user interface using ", key K.space, cont " leaving only the canvas visible"
        , cont " (and get it back by pressing ", key K.space, cont " again)"
        ]
    GeneralInterface ToggleSolidBackground ->
        -11 /\
        [ st, cont "Press ", key (K.w_shift $ K.space), cont " to toggle solid background"
        ]

    GeneralInterface ToggleTransparentBackground ->
        -11 /\
        [ st, cont "Press ", key (K.w_shift $ K.space), cont " to toggle transparent background"
        ]

    GeneralInterface ShowInterface ->
        -11 /\
        [ st, cont "Show the user interface using ", key K.space
        ]
    GeneralInterface StepBackInKeyboardCombo ->
        0 /\
        [ st, cont "Press ", key $ K.backspace, cont " to step back in the keyboard combo sequence"
        ]
    GeneralInterface ObserveKeyboardCombo ->
        -10 /\
        [ st, cont "You can track the keyboard combo sequence in the status bar"
        ]

    {- Library -}

    Library SpawnByFamily ->
        case controller of
            Mouse ->
                -5 /\
                [ st, cont "Spawn a node by ", mclick "clicking", cont " the desired family in the Library on the left"
                , cont "You can scroll the list using ", mwheel "mouse wheel", cont " or trackpad"
                ]
            Keyboard ->
                -5 /\
                [ st, cont "Use keyboard key ", key $ K.key "l", cont " to focus on the Library"
                ]
    Library SelectFamilyToSpawn ->
         1 /\
        [ st, cont "Press the ", hindex Family, cont " that is shown close to the family name and press ", key K.enter, cont " to confirm"
        ]
    Library ConfirmFamilyToSpawn ->
         1 /\
        [ st, cont "Press ", key K.enter, cont " to spawn the node of the selected family"
        ]

    {- CommandInput -}

    CommandInput LaunchCommandInput ->
        -5 /\
        [ st, cont "Call command input by pressing ", key K.tab ]
    CommandInput EnterCommand ->
         3 /\
        [ st, cont "Try typing the node family and pressing ", key K.enter, cont ", for example ", code "osc", cont " will spawn the corresponding node"
        , st, cont "You can also create custom nodes by typing something like ", code ":: <a:Number -> b:Number -> c:Number> => <out>", cont " or ", code ":: <tuple> => <fst -> snd>", cont ", just start with double-semicolon"
        , st, cont "To close the input without applying, press ", key K.escape
        , st, cont "To apply the command, press ", key K.enter, cont " after entering it"
        ]

    {- Patches -}

    Patches CreatePatch ->
        -- [ st, cont "Create a patch by selecting multiple nodes (hold ", key K.shift, cont " and click on nodes), then press ", key $ K.key "p"
        -- ]
        -8 /\
        [ st, cont "Create a new patch by ", mclick "clicking", cont " (+) button in the patches bar"
        ]
    Patches SelectPatch ->
        -8 /\
        [ st, cont "Select a patch by ", mclick "clicking", cont " on its name in the patches bar"
        ]

    {- One node -}

    PatchArea (OneNode AddToSelection) ->
        case controller of
            Mouse ->
                -5 /\
                [ st, cont "To select a node, ", mhover "hover", cont " over it and ", mclick "click", cont " on its body" ]
            Keyboard ->
                2 /\
                [ st, cont "Press ", key $ K.key "n", cont " to start selecting nodes using keyboard" ]
    PatchArea (OneNode SelectInletsOrOutlets) ->
        5 /\
        [ st, cont "Press ", key $ K.key "i", cont " to start selecting the particular inlet."
        , st, cont "Press ", key $ K.key "o", cont " to start selecting the particular outlet"
        ]
    PatchArea (OneNode SelectInlet) ->
        5 /\
        [ st, cont "Press the corresponding ", hindex Inlet, cont " that is shown close in the connector near the inlet name"
        ]
    PatchArea (OneNode SelectOutlet) ->
        5 /\
        [ st, cont "Press the corresponding ", hindex Outlet, cont " that is shown close in the connector near the outlet name"
        ]
    PatchArea (OneNode EditInletValue) ->
        case controller of
            Mouse ->
                -5 /\
                [ st, cont "To change the value at the inlet, ", mclick "click", cont " on it and the value editor will open"
                ]
            Keyboard ->
                3 /\
                [ st, cont "To edit the inlet value, press " , key K.enter, cont " and type the desired value"
                ]
    PatchArea (OneNode FinishEditingInletValue) ->
        6 /\
        [ st, cont "To close the value editor, place ", key K.escape, cont " or ", key K.enter
        ]

    {- Some nodes -}

    PatchArea (SomeNodes DragNodes) ->
        case controller of
            Mouse ->
                -10 /\
                [ st, cont "To drag a node, hover over it and ", mclick "click", cont " the four-arrow button, then drag to the desired position and confirm by ", mclick "clicking", cont " again"
                ]
            Keyboard ->
                -20 /\ [ ]
    PatchArea (SomeNodes FinishDraggingNodes) ->
        0 /\
        [ st, cont "Drag the node to the desired position and confirm by ", mclick "clicking", cont " the four-arrow button again"
        ]
    PatchArea (SomeNodes MoveNodes) ->
        case controller of
            Mouse ->
                -20 /\ [ ]
            Keyboard ->
                5 /\
                [ st, cont "Move the node using ", key K.arrows, cont ". To move faster, hold ", key K.shift, cont " while pressing the  ", key K.arrows
                ]
    PatchArea (SomeNodes ConfirmMovingNodes) ->
        5 /\
        [ st, cont "Press ", key K.enter, cont " to confirm the position"
        ]
    PatchArea (SomeNodes CancelMovingNodes) ->
        -1 /\
        [ st, cont "Press ", key K.escape, cont " to move the node back to its previous place"
        ]
    PatchArea (SomeNodes DeselectNodes) ->
        0 /\
        [ st, cont "To deselect a node, ", mclick "click", cont " on it"
        ]
    PatchArea (SomeNodes DeleteNodes) ->
        case controller of
            Mouse ->
                0 /\
                [ st, cont "To remove some node, ", mhover "hover", cont " over it and ", mclick "click", cont " the cross button." ]
            Keyboard ->
                0 /\
                [ st, cont "To remove some node using keyboard, press ", key K.delete, cont " or ", key $ K.key "x"
                ]

    {- Patch area -}

    PatchArea SelectNodeToConnectFrom ->
        case controller of
            Mouse ->
                -1 /\
                [ st, cont "To start connecting nodes, ", mclick "click", cont " on the desired outlet where the link should start"
                ]
            Keyboard ->
                -3 /\
                [ st, cont "To connect nodes with keyboard, select the node to connect from using ", seq [ skey $ K.key "n", sindex Node, so ]
                , st, cont "then select the outlet using ", seq [ so, skey $ K.key "o", sindex Outlet, so ], cont ", and follow further instructions"
                ]
    PatchArea SelectOutletToConnectFrom ->
        case controller of
            Mouse ->
                -1 /\
                [ st, cont "To start connecting nodes, ", mclick "click", cont " on the desired outlet where the link should start"
                ]
            Keyboard ->
                5 /\
                [ st, cont "Select the outlet using ", seq [ so, skey $ K.key "o", sindex Outlet, so ], cont ", and follow further instructions"
                ]
    PatchArea SelectNodeToConnectTo ->
        case controller of
            Mouse ->
                4 /\
                [ st, cont "To finish creating a link, ", mclick "click", cont " on the inlet you want to connect to"
                ]
            Keyboard ->
                6 /\
                [ st, cont "select the node to connect to using ", seq [ so, sindex Node, so ]
                ]
    PatchArea SelectInletToConnectTo ->
        case controller of
            Mouse ->
                4 /\
                [ st, cont "To finish creating a link, ", mclick "click", cont " on the inlet you want to connect to"
                ]
            Keyboard ->
                6 /\
                [ st, cont "select the inlet to connect to using ", seq [ so, skey $ K.key "i", sindex Inlet ]
                ]
    PatchArea ConfirmConnectingNodes ->
        -20 /\
        [ ]
    PatchArea CancelConnectingNodes ->
        4 /\
        [ st, cont "To cancel creating link, ", mclick "click", cont " somewhere on the empty area or press ", key K.escape
        ]
    PatchArea ChangeZoom ->
        -5 /\
        [ st, cont "Zoom with ", seq [ skey K.shift, smwheel "mouse wheel" ], cont " or ", key $ K.w_ctrl K.plus, cont "/", key $ K.w_ctrl K.minus
        ]
    PatchArea ResetZoom ->
        -7 /\
        [ st, cont "Reset zoom by ", mclick "clicking", cont " (*) in the status bar"
        ]
    PatchArea DisconnectLink ->
        -1 /\
        [ st, cont "To disconnect a link, just ", mclick "click", cont " somewhere on its shape"
        ]
    PatchArea HoverForDocumentation ->
        -4 /\
        [ st, mhover "Hover", cont " over inlets, outlets, nodes, to get the information in status bar"
        ]


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