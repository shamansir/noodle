module Web.Components.AppScreen.KeyboardLogic where

import Prelude

import Blessed.UI.Base.Element.Option (input)
import Data.Array (head, takeEnd) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Either as Either
import Data.Enum (fromEnum, toEnum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.CodePoints as CP
import Data.Tuple.Nested ((/\), type (/\))
import Front.Shared.HelpText (Context(..), empty) as HelpText
import Front.Shared.HelpText (class ProvidesHelp)
import Front.Shared.HelpText as HT
import Noodle.Network (hasPatch)
import Web.Components.AppScreen.UiMode (UiMode(..)) as UiMode
import Web.Components.AppScreen.UiMode (UiMode)
import Web.UIEvent.KeyboardEvent as KE

-- import Web.Components.PatchArea.Types (LockingTask) as PA


data Focus
    = Free
    | CommandInput
    | ValueEditor
    | Library
    | LibraryFamily Int
    -- | SidePanels
    -- | SidePanel Int
    | PatchesBar
    | Patch Int
    | NodesArea
    | Node Int
    | NodeInlets Int
    | NodeOutlets Int
    | NodeInlet (Int /\ Int)
    | NodeOutlet (Int /\ Int)
    | Connecting (Int /\ Int) ConnectTo
    | SomeNodes (NonEmptyArray Int)


data NodeFocus
    = NoFocusedNode
    | NodeSelected
    | NodeOpen Int
    | NodeSemiOpen Int
    | InletsOpen
    | OutletsOpen
    | InletSelected Int
    | OutletSelected Int


data LibraryFocus
    = NoFocusedFamily
    | LibraryOpen
    | FamilySelected Int


data ConnectTo
    = NoTarget
    | ToNode Int
    | ToInlet (Int /\ Int)


type Input =
    { uiMode :: UiMode
    , nodesCount :: Int
    , linksCount :: Int
    , familiesCount :: Int
    , patchesCount :: Int
    , mbCurrentNode :: Maybe { inletsCount :: Int, outletsCount :: Int }
    , valueEditorOpened :: Boolean
    , zoomChanged :: Boolean
    -- , patchLock :: PA.LockingTask
    }


type State =
    { shiftPressed :: Boolean
    , focus :: Focus
    -- , nodeStats :: Maybe { inlets :: Int, outlets :: Int}
    -- , selectedNodes :: Array Int
    }


init :: State
init =
    { shiftPressed : false
    , focus : Free
    }


data Dir
    = DLeft
    | DUp
    | DDown
    | DRight


instance Show Dir where
    show = case _ of
        DLeft -> "←"
        DUp -> "↑"
        DDown -> "↓"
        DRight -> "→"


newtype FamilyIndex = FamilyIndex Int
newtype NodeIndex = NodeIndex Int
newtype InletIndex = InletIndex Int -- TODO: duplicates `Id.InletIndex``
newtype OutletIndex = OutletIndex Int -- TODO: duplicates `Id.OutletIndex``


data Action
    -- = OpenCommandInput
    -- | CloseCommandInput
    -- | StopListeningKeyboard
    -- | OpenValueEditor
    = OpenValueEditor NodeIndex InletIndex
    | CloseValueEditor
    | CloseCommandInput
    -- | MoveNode Int Dir
    -- | FocusOn Focus
    -- | ClearFocus
    -- | SpawnFromLibrary
    -- | AddToSelection
    -- | RemoveFromSelection
    -- | ClearSelection
    -- | ZoomIn
    -- | ZoomOut
    -- |StartConnectingNodes
    | CancelConnectingNodes
    | ChangeUiMode UiMode
    | SpawnNode FamilyIndex
    | MoveNode NodeIndex Dir
    | RemoveNode NodeIndex
    | StartConnecting NodeIndex OutletIndex
    | FinishConnecting NodeIndex OutletIndex NodeIndex InletIndex
    -- |
    -- |


loadNodeFocus :: Int -> Focus -> NodeFocus
loadNodeFocus nodeIdx = case _ of
    NodesArea           -> NodeOpen nodeIdx
    Node n              -> if n == nodeIdx then NodeSelected else NoFocusedNode
    NodeInlets n        -> if n == nodeIdx then InletsOpen else NoFocusedNode
    NodeOutlets n       -> if n == nodeIdx then OutletsOpen else NoFocusedNode
    NodeInlet  (n /\ i) -> if n == nodeIdx then InletSelected i else NoFocusedNode
    NodeOutlet (n /\ o) -> if n == nodeIdx then OutletSelected o else NoFocusedNode
    Connecting (n /\ o)
            NoTarget    -> if n == nodeIdx then OutletSelected o else NodeOpen nodeIdx
    Connecting (n /\ o)
            (ToNode n') -> if n == nodeIdx then OutletSelected o else
                           if (n' == nodeIdx) then InletsOpen else NoFocusedNode
    Connecting (n /\ o)
            (ToInlet (n' /\ i))
                        -> if n == nodeIdx then OutletSelected o else
                           if (n' == nodeIdx) then InletSelected i else NoFocusedNode
    SomeNodes nodes     -> if NEA.elem nodeIdx nodes then NodeSemiOpen nodeIdx else NodeOpen nodeIdx
    _ -> NoFocusedNode


loadLibraryFocus :: Focus -> LibraryFocus
loadLibraryFocus = case _ of
    Library -> LibraryOpen
    LibraryFamily f -> FamilySelected f
    _ -> NoFocusedFamily


trackKeyDown :: Input -> State -> KE.KeyboardEvent -> State /\ Array Action
trackKeyDown input state kevt =
    let
        keyName = String.toLower $ KE.key kevt
        keyCode = String.toLower $ KE.code kevt
        shiftPressed = KE.shiftKey kevt
        controlPressed = KE.ctrlKey kevt
        nextState = state { shiftPressed = shiftPressed }
        valueEditorOpened = input.valueEditorOpened -- FIXME: not in sync with nextState.focus, check:
            -- case nextState.focus of
            --     ValueEditor -> true
            --     _ -> false
        someTextInputOpened = case nextState.focus of
            CommandInput -> true
            ValueEditor -> true
            _ -> false
    in
        if (keyName == "escape") then
            -- cancel all operations on escape
            ( nextState
               { focus = Free
               }
            )
            /\ [ CancelConnectingNodes, CloseValueEditor, CloseCommandInput ] -- TODO: match to current focus, dont't cancel all blindly

        else if (valueEditorOpened && keyName == "enter" && not shiftPressed) then
            -- cancel operations when enter was pressed without `shift` while editing text
            ( nextState
               { focus = Free
               }
            )
            /\ [ CloseValueEditor ]
             -- FIXME: command input is closed by UI separately, should be moved here in logic

        else if (keyName == "tab") then -- FIXME: the browser owns focus by Tab
            (case nextState.focus of
                Free ->
                    ( nextState
                        { focus = CommandInput }
                    ) /\ [ ]
                CommandInput ->
                    ( nextState
                        { focus = Free }
                    ) /\ [ ]
                _ ->
                    nextState /\ []
            )

        else if someTextInputOpened then -- keys should not affect logic when user enters something in text input
            nextState /\ []

        else if (keyCode == "space") then
            nextState
            /\
            [ ChangeUiMode
            $ if not shiftPressed then
                case input.uiMode of
                      UiMode.OnlyCanvas prev -> prev
                      other -> UiMode.OnlyCanvas other
              else

                  case input.uiMode of
                      UiMode.SolidOverlay prev -> prev
                      other -> UiMode.SolidOverlay other
            ]

        else if (keyCode == "backspace") then

             nextState
                { focus = tryLevelUp nextState.focus }
             /\ []

        else if keyCode == "enter" then

            case nextState.focus of
                ValueEditor ->
                    nextState /\ if input.valueEditorOpened then [ CloseValueEditor ] else []
                LibraryFamily idx ->
                    nextState
                        { focus = Free }
                    /\ [ SpawnNode $ FamilyIndex idx ]
                Connecting (nidx /\ oidx)
                    (ToInlet (nidx' /\ iidx)) ->
                    nextState
                        { focus = Free }
                    -- /\ [ FinishConnecting (NodeIndex nidx) (OutletIndex oidx) (NodeIndex nidx') (InletIndex iidx) ]
                    /\ []
                NodeInlet (n /\ i) ->
                    nextState
                    /\ [ if not input.valueEditorOpened
                          then OpenValueEditor (NodeIndex n) (InletIndex i)
                          else CloseValueEditor
                       ]
                _ -> nextState /\ []

        -- since the index can be a letter, as well as commands, we should re-check
        else if not waitingForIndex nextState.focus then
            (

            if (keyName == "n") then
                ( nextState
                    { focus = NodesArea }
                ) /\ [ ]

            else if (keyName == "i") then
                case nextState.focus of
                    Node n ->
                        ( nextState
                            { focus = NodeInlets n }
                        ) /\ [ ]
                    _ -> nextState /\ []

            else if (keyName == "o") then
                case nextState.focus of
                    Node n ->
                        ( nextState
                            { focus = NodeOutlets n }
                        ) /\ [ ]
                    _ -> nextState /\ []

            else if (keyName == "l") then
                ( nextState
                    { focus = Library }
                ) /\ [ ]

            else if (keyName == "p") then
                ( nextState
                    { focus = PatchesBar }
                ) /\ [ ]

            else if (keyName == "a") then
                ( nextState
                    { focus = case nextState.focus of
                        Node nidx -> SomeNodes $ NEA.singleton nidx
                        _ -> nextState.focus
                    }
                ) /\ [ ]

            else if (keyName == "c") then
                case nextState.focus of
                    NodeOutlet (nidx /\ oidx) ->
                        nextState
                            { focus = Connecting (nidx /\ oidx) NoTarget }
                        /\ [ StartConnecting (NodeIndex nidx) (OutletIndex oidx) ]
                    _ -> nextState /\ []

            else if (keyName == "d") || (keyName == "r") then
                case nextState.focus of
                    Node nidx -> nextState { focus = Free } /\ [ RemoveNode $ NodeIndex nidx ]
                    _ -> nextState /\ []

            else

                case keyToDir kevt /\ nextState.focus of

                    Just dir /\ Node n ->
                       nextState /\ [ MoveNode (NodeIndex n) dir ]

                    _ -> nextState /\ []

            )

        else

            case Either.choose (keyToDir kevt) (keyToNum kevt) of

                Just eitherNav ->
                    let nextState' = nextState { focus = navigateIfNeeded eitherNav input nextState.focus }
                    in nextState' /\ case (nextState.focus /\ nextState'.focus) of
                           Connecting _ (ToNode _) /\ Connecting (nidx /\ oidx) (ToInlet (nidx' /\ iidx)) ->
                                [ FinishConnecting (NodeIndex nidx) (OutletIndex oidx) (NodeIndex nidx') (InletIndex iidx) ]
                           _ -> []

                Nothing -> nextState /\ []

instance Show Focus where
    show = focusToString


trackKeyUp :: Input -> State -> KE.KeyboardEvent -> State /\ Array Action
trackKeyUp input state kevt = state { shiftPressed = KE.shiftKey kevt } /\ []


canZoom :: State -> Boolean
canZoom = _.shiftPressed


isCommandInputOpen :: State -> Boolean
isCommandInputOpen = _.focus >>> case _ of
    CommandInput -> true
    _ -> false


commandInputClosed :: State -> State
commandInputClosed = _ { focus = Free } -- TODO: return to previous focus, also it feels strange modifying it here


waitingForIndex:: Focus -> Boolean
waitingForIndex = case _ of
    Free -> false -- waits for command to choose focus
    NodesArea -> true -- for node index
    PatchesBar -> true -- for patch index
    Library -> true -- for family index
    Node _ -> false -- waits for inlet/outlet command
    NodeInlets _ -> true -- for inlet index
    NodeOutlets _ -> true -- for outlet index
    NodeInlet _ -> false -- waits for connect or edit command
    NodeOutlet _ -> false -- waits for connect or edit command
    LibraryFamily _ -> false -- waits for spawn command
    Patch _ -> false -- waits for select command
    CommandInput -> false -- waits for text to be entered
    ValueEditor -> false -- waits for text to be entered
    Connecting _ NoTarget  -> true -- waits for next node index
    Connecting _ (ToNode _) -> true -- waits fot inlet index
    Connecting _ (ToInlet _) -> false -- the connection is done
    SomeNodes _ -> true -- waits for next nodes indices


selectedNode :: State -> Maybe Int
selectedNode = _.focus >>> case _ of
    Node n -> Just n
    NodeInlets n -> Just n
    NodeOutlets n -> Just n
    NodeInlet  (n /\ _) -> Just n
    NodeOutlet (n /\ _) -> Just n
    Connecting (n /\ _) NoTarget -> Just n
    Connecting (_ /\ _) (ToNode n') -> Just n'
    Connecting (_ /\ _) (ToInlet (n' /\ _)) -> Just n'
    SomeNodes nodes -> Array.head $ NEA.takeEnd 1 nodes -- FIXME:
    _ -> Nothing


navigateIfNeeded :: Either Dir Int -> Input -> Focus -> Focus
navigateIfNeeded (Right num) input curFocus =
    case curFocus of
        Library         -> LibraryFamily $ min num (input.familiesCount - 1)
        LibraryFamily _ -> LibraryFamily $ min num (input.familiesCount - 1)
        PatchesBar      -> Patch $ min num (input.patchesCount - 1)
        Patch _         -> Patch $ min num (input.patchesCount - 1)
        NodesArea       -> Node $ min num (input.nodesCount - 1)
        Node _          -> Node $ min num (input.nodesCount - 1)
        NodeInlet _     -> curFocus
        NodeOutlet _    -> curFocus
        NodeInlets  nodeIdx -> NodeInlet  $ nodeIdx /\ (min num $ fromMaybe 0 $ _.inletsCount  <$> input.mbCurrentNode)
        NodeOutlets nodeIdx -> NodeOutlet $ nodeIdx /\ (min num $ fromMaybe 0 $ _.outletsCount <$> input.mbCurrentNode)
        Connecting (n /\ o)
                   NoTarget -> Connecting (n /\ o) $ ToNode $ min num (input.nodesCount - 1)
        Connecting (n /\ o)
                (ToNode n') -> Connecting (n /\ o) $ ToInlet (n' /\ (min num $ fromMaybe 0 $ _.inletsCount <$> input.mbCurrentNode))
        Connecting (n /\ o)
                (ToInlet _) -> Free -- TODO: investigate, why not keep current focus
        Free -> curFocus
        CommandInput -> curFocus
        ValueEditor -> curFocus
        SomeNodes nodes -> SomeNodes $ NEA.snoc nodes $ min num (input.nodesCount - 1)

navigateIfNeeded (Left dir)  input state = state -- TODO: implement


keyToDir :: KE.KeyboardEvent -> Maybe Dir
keyToDir = KE.code >>> String.toLower >>> case _ of
    "arrowleft" -> Just DLeft
    "arrowright" -> Just DRight
    "arrowup" -> Just DUp
    "arrowdown" -> Just DDown
    _ -> Nothing


keyToNum :: KE.KeyboardEvent -> Maybe Int
keyToNum =
    -- KE.which >>> ?wh
  KE.code >>> String.toLower >>> case _ of
    "digit0" -> Just 0
    "digit1" -> Just 1
    "digit2" -> Just 2
    "digit3" -> Just 3
    "digit4" -> Just 4
    "digit5" -> Just 5
    "digit6" -> Just 6
    "digit7" -> Just 7
    "digit8" -> Just 8
    "digit9" -> Just 9
    "keya"   -> Just 10
    "keyb"   -> Just 11
    "keyc"   -> Just 12
    "keyd"   -> Just 13
    "keye"   -> Just 14
    "keyf"   -> Just 15
    "keyg"   -> Just 16
    "keyh"   -> Just 17
    "keyi"   -> Just 18
    "keyj"   -> Just 19
    "keyk"   -> Just 20
    "keyl"   -> Just 21
    "keym"   -> Just 22
    "keyn"   -> Just 23
    "keyo"   -> Just 24
    "keyp"   -> Just 25
    "keyq"   -> Just 26
    "keyr"   -> Just 27
    "keys"   -> Just 28
    "keyt"   -> Just 29
    "keyu"   -> Just 30
    "keyv"   -> Just 31
    "keyw"   -> Just 32
    "keyx"   -> Just 33
    "keyy"   -> Just 34
    "keyz"   -> Just 35
    _ -> Nothing



tryLevelUp :: Focus -> Focus
tryLevelUp = case _ of
    Free -> Free
    CommandInput -> CommandInput
    ValueEditor -> ValueEditor
    Library -> Free
    LibraryFamily _ -> Library
    PatchesBar -> Free
    Patch _ -> PatchesBar
    NodesArea -> Free
    Node _ -> NodesArea
    NodeInlets n -> Node n
    NodeOutlets n -> Node n
    NodeInlet (n /\ _) -> NodeInlets n
    NodeOutlet (n /\ _) -> NodeOutlets n
    Connecting (n /\ o) NoTarget -> NodeOutlet (n /\ o)
    Connecting (n /\ o) (ToNode _) -> Connecting (n /\ o) NoTarget
    Connecting (n /\ o) (ToInlet (n' /\ _)) -> Connecting (n /\ o) (ToNode n')
    SomeNodes nodes -> case NEA.fromArray $ NEA.dropEnd 1 nodes of
        Just previousNodes -> SomeNodes previousNodes
        Nothing -> Free

{-
    GlobalKeyDown kevt -> do
        let keyName = String.toLower $ KE.key kevt
        let keyCode = String.toLower $ KE.code kevt
        let shiftPressed = KE.shiftKey kevt
        let controlPressed = KE.ctrlKey kevt
        H.modify_ $ _ { shiftPressed = shiftPressed }
        when (keyName == "escape") $ do
            H.tell _patchArea SVG PatchArea.CancelConnecting
            H.tell _patchArea HTML PatchArea.ValueEditorClosedByUser
        when (keyName == "tab") $ do
            H.modify_ \s -> s { commandInputActive = not s.commandInputActive }
        when (keyCode == "space") $ do
            H.modify_ \s -> s { uiMode =
                if not shiftPressed then
                    case s.uiMode of
                        CState.OnlyCanvas prev -> prev
                        other -> CState.OnlyCanvas other
                else
                    case s.uiMode of
                        CState.SolidOverlay prev -> prev
                        other -> CState.SolidOverlay other
            }
            nextMode <- _.uiMode <$> H.get
            case nextMode of
                CState.OnlyCanvas _ -> pure unit
                _ -> handleAction ploc LoadCurrentPatchChanges
        when ((keyName == "h") && controlPressed) $ do
            H.modify_ \s -> s { helpText = not s.helpText }
    GlobalKeyUp kevt ->
        H.modify_ $ _ { shiftPressed = KE.shiftKey kevt }

-}


digit0Pos = 48 :: Int
digit9Pos = 57 :: Int
upperAPos = 65 :: Int
upperZPos = 90 :: Int
lowerAPos = 97 :: Int
lowerZPos = 122 :: Int


indexToChar :: Int -> String
indexToChar idx =
    if idx < 10 then
        show idx
    else if idx < (10 + 26) then
        toEnum (idx - 10 + lowerAPos) <#> CP.singleton # fromMaybe "."
    -- else if idx < (10 + 26 * 2) then
    --    toEnum (idx - 10 + lowerAPos) <#> CP.singleton # fromMaybe "."
    else "."


whichToIndex :: Int -> Int
whichToIndex which =
    if which >= digit0Pos && which <= digit9Pos then which - digit0Pos
    else if which >= upperAPos && which <= upperZPos then 10 + which - upperAPos
    else if which >= lowerAPos && which <= lowerZPos then 10 + which - lowerAPos
    else -1


focusToString :: Focus -> String
focusToString = case _ of
    Free -> "[ ]"
    CommandInput -> "CMD"
    ValueEditor -> "EDIT"
    PatchesBar -> "PTCH"
    Patch pr -> "PTCH" <> show pr
    Library -> "LIB"
    LibraryFamily f -> "LIB-" <> indexToChar f
    NodesArea -> "NOD"
    Node n -> "NOD-" <> indexToChar n
    NodeInlets n -> "NOD-" <> indexToChar n <> "-I-"
    NodeOutlets n -> "NOD-" <> indexToChar n <> "-O-"
    NodeInlet (n /\ i) -> "NOD-" <> indexToChar n <> "-I-" <> indexToChar i
    NodeOutlet (n /\ o) -> "NOD-" <> indexToChar n <> "-O-" <> indexToChar o
    Connecting (n /\ o)
        NoTarget              -> "NOD-" <> indexToChar n <> "-O-" <> indexToChar o <> "-CON"
    Connecting (n /\ o)
        (ToNode n')        -> "NOD-" <> indexToChar n <> "-O-" <> indexToChar o <> "-CON-NOD-" <> indexToChar n'
    Connecting (n /\ o)
        (ToInlet (n' /\ i))   -> "NOD-" <> indexToChar n <> "-O-" <> indexToChar o <> "-CON-NOD-" <> indexToChar n' <> "-I-" <> indexToChar i
    SomeNodes nodes -> "NOD-" <> indexToChar (NEA.head nodes) <> "-A-" <> (String.joinWith "-" $ indexToChar <$> NEA.tail nodes)


toSequence :: Focus -> Array String
toSequence = case _ of
    Free -> []
    CommandInput ->              [ "CMD" ]
    ValueEditor ->               [ "VAL" ]
    Library ->                   [ "l" ]
    LibraryFamily fidx ->        [ "l", show fidx ]
    PatchesBar ->                [ "p" ]
    Patch pidx ->                [ "p", show pidx ]
    NodesArea ->                 [ "n" ]
    Node nidx ->                 [ "n", show nidx ]
    NodeInlets nidx ->           [ "n", show nidx, "i" ] -- ⊥
    NodeOutlets nidx ->          [ "n", show nidx, "o" ] -- ⊤
    NodeInlet  (nidx /\ iidx) -> [ "n", show nidx, "i", show iidx ]
    NodeOutlet (nidx /\ oidx) -> [ "n", show nidx, "o", show oidx ]
    Connecting (nidx /\ oidx)
        NoTarget              -> [ "n", show nidx, "o", show oidx, "c" ]
    Connecting (nidx /\ oidx)
        (ToNode nidx')        -> [ "n", show nidx, "o", show oidx, "c", "n", show nidx' ]
    Connecting (nidx /\ oidx)
        (ToInlet (nidx' /\ iidx))
                              -> [ "n", show nidx, "o", show oidx, "c", "n", show nidx', "i", show iidx ]
    SomeNodes nodes ->
        [ "n", show (NEA.head nodes), "a" ] <> (show <$> NEA.tail nodes)


resetFocus :: State -> State
resetFocus = _ { focus = Free }


nextActions :: Input -> State -> Array HT.PossibleAction
nextActions input { focus } =
    case focus of
        Free ->
            when input.zoomChanged  [ HT.PatchArea HT.KB_ResetZoom ]
            <>
                [ HT.CommandInput HT.KB_LaunchCommandInput
                , HT.Patches HT.KB_CreatePatch
                , HT.PatchArea HT.KB_ChangeZoom
                ]
            <>
                when hasFamilies
                    [ HT.Library HT.KB_FocusOnLibrary ]
            <>
                when hasPatches
                    [ HT.Patches HT.KB_SelectPatch ]
            <>
                when hasNodes
                    [ HT.PatchArea HT.M_HoverForDocumentation
                    , HT.PatchArea $ HT.G_OneNode HT.M_AddToSelection
                    , HT.PatchArea $ HT.G_OneNode HT.KB_StartChoosingNodes
                    -- , HT.PatchArea $ HT.G_SomeNodes HT.KB_DeleteNodes
                    , HT.PatchArea $ HT.G_SomeNodes HT.M_DeleteNodes
                    , HT.PatchArea $ HT.G_SomeNodes HT.M_DragNodes
                    ]
            <>
                when hasLinks
                    [ HT.PatchArea HT.KB_DisconnectLink ]
            -- [ HT.PatchArea HT.ChangeZoom ]
        CommandInput ->
            [ HT.CommandInput HT.KB_EnterCommand ]
        ValueEditor ->
            [ HT.PatchArea $ HT.G_OneNode HT.KB_FinishEditingInletValue ]
        Library ->
            when hasFamilies
            [ HT.Library HT.KB_SelectFamilyToSpawn ]
            <>
            comboBackAndCancel
        LibraryFamily _ ->
            when hasFamilies
            [ HT.Library HT.KB_ConfirmFamilyToSpawn ]
            <>
            comboBackAndCancel
        PatchesBar ->
            [ HT.Patches HT.KB_CreatePatch
            ] <>
            when hasPatches
                [ HT.Patches HT.KB_SelectPatch ]
        Patch _ ->
            [ ]
        NodesArea ->
            when hasNodes
                [ HT.PatchArea $ HT.G_OneNode HT.KB_ChooseNodeByIndex ]
            <>
            comboBackAndCancel
        Node _ ->
            when hasNodes
                [ HT.PatchArea $ HT.G_OneNode HT.KB_SelectInletsSide
                , HT.PatchArea $ HT.G_OneNode HT.KB_SelectOutletsSide
                , HT.PatchArea $ HT.G_SomeNodes HT.KB_DeleteNodes
                , HT.PatchArea $ HT.G_SomeNodes HT.KB_MoveNodes
                ]
            <>
            comboBackAndCancel
        NodeInlets nidx ->
            when hasInlets
            [ HT.PatchArea $ HT.G_OneNode HT.KB_SelectInlet ]
            <>
            comboBackAndCancel
        NodeOutlets nidx ->
            when hasOutlets
            [ HT.PatchArea $ HT.G_OneNode HT.KB_SelectOutlet ]
            <>
            comboBackAndCancel
        NodeInlet (nidx /\ iidx) ->
            [ HT.PatchArea $ HT.G_OneNode HT.KB_EditInletValue ]
            <>
            comboBackAndCancel
        NodeOutlet (nidx /\ oidx) ->
            when hasNodes
            [ HT.PatchArea $ HT.KB_SelectNodeToConnectTo ]
            <>
            comboBackAndCancel
        Connecting (nidx /\ oidx) NoTarget ->
            when hasMoreThanOneNode
            [ HT.PatchArea $ HT.KB_SelectNodeToConnectTo
            , HT.PatchArea $ HT.KB_CancelConnectingNodes
            ]
            <>
            comboBackAndCancel
        Connecting (nidx /\ oidx) (ToNode nidx') ->
            when hasMoreThanOneNode
            [ HT.PatchArea $ HT.KB_SelectInletToConnectTo
            , HT.PatchArea $ HT.KB_CancelConnectingNodes
            ]
            <>
            comboBackAndCancel
        Connecting (nidx /\ oidx) (ToInlet (nidx' /\ iidx)) ->
            when hasMoreThanOneNode
            [ HT.PatchArea $ HT.KB_ConfirmConnectingNodes
            ]
                        <>
            comboBackAndCancel
        SomeNodes nodes ->
            when hasNodes
            [ HT.PatchArea $ HT.G_OneNode HT.KB_ChooseNodeByIndex ]
            <>
            comboBackAndCancel
        where
            hasNodes = input.nodesCount > 0
            hasLinks = input.linksCount > 0
            hasPatches = input.patchesCount > 0
            hasFamilies = input.familiesCount > 0
            hasInlets  = input.mbCurrentNode <#> _.inletsCount  <#> (_ > 0) # fromMaybe false
            hasOutlets = input.mbCurrentNode <#> _.outletsCount <#> (_ > 0) # fromMaybe false
            hasMoreThanOneNode = input.nodesCount > 1
            comboBackAndCancel =
                [ HT.GeneralInterface HT.KB_StepBackInKeyboardCombo
                , HT.GeneralInterface HT.KB_CancelKeyboardCombo
                ]
            when cond actions = if cond then actions else []