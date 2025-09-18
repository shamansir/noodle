module Web.Components.AppScreen.KeyboardLogic where

import Prelude

import Debug as Debug

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Either as Either
import Data.String as String
import Data.Enum (fromEnum, toEnum)
import Data.String.CodePoints as CP
import Data.Tuple.Nested ((/\), type (/\))

import Web.Components.AppScreen.UiMode (UiMode(..)) as UiMode
import Web.Components.AppScreen.UiMode (UiMode)

import Web.UIEvent.KeyboardEvent as KE


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
    | NodeInlet Int Int
    | NodeOutlet Int Int


data NodeFocus
    = NoFocusedNode
    | NodeSelected
    | NodeOpen Int
    | InletsOpen
    | OutletsOpen
    | InletSelected Int
    | OutletSelected Int


data LibraryFocus
    = NoFocusedFamily
    | LibraryOpen
    | FamilySelected Int


type Input =
    { uiMode :: UiMode
    , nodesCount :: Int
    , familiesCount :: Int
    , patchesCount :: Int
    , mbCurrentNode :: Maybe { inletsCount :: Int, outletsCount :: Int }
    }


type State =
    { notListening :: Boolean
    , shiftPressed :: Boolean
    , focus :: Focus
    -- , nodeStats :: Maybe { inlets :: Int, outlets :: Int}
    -- , selectedNodes :: Array Int
    }


init :: State
init =
    { notListening : false
    , shiftPressed : false
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


data Action
    -- = OpenCommandInput
    -- | CloseCommandInput
    -- | StopListeningKeyboard
    -- | OpenValueEditor
    = CloseValueEditor
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
    -- |
    -- |


loadNodeFocus :: Int -> Focus -> NodeFocus
loadNodeFocus nodeIdx = case _ of
    NodesArea -> NodeOpen nodeIdx
    Node n -> if n == nodeIdx then NodeSelected else NoFocusedNode
    NodeInlets n  -> if n == nodeIdx then InletsOpen else NoFocusedNode
    NodeOutlets n -> if n == nodeIdx then OutletsOpen else NoFocusedNode
    NodeInlet n i -> if n == nodeIdx then InletSelected i else NoFocusedNode
    NodeOutlet n o -> if n == nodeIdx then OutletSelected o else NoFocusedNode
    _ -> NoFocusedNode


loadLibraryFocus :: Focus -> LibraryFocus
loadLibraryFocus = case _ of
    Library -> LibraryOpen
    LibraryFamily f -> FamilySelected f
    _ -> NoFocusedFamily



trackKeyDown :: Input -> State -> KE.KeyboardEvent -> State /\ Array Action
trackKeyDown input state kevt =
    let
        keyName = Debug.spy "name" $ String.toLower $ KE.key kevt
        keyCode = Debug.spy "code" $ String.toLower $ KE.code kevt
        shiftPressed = KE.shiftKey kevt
        controlPressed = KE.ctrlKey kevt
        nextState = state { shiftPressed = shiftPressed }
    in
        if (keyName == "escape") then
            ( nextState
               { focus = Free
               , notListening = false
               }
            )
            /\ [ CancelConnectingNodes, CloseValueEditor ]

        else if (keyName == "tab") then
            (case nextState.focus of
                Free ->
                    ( nextState
                        { focus = CommandInput
                        , notListening = true
                        }
                    ) /\ [ ]
                CommandInput ->
                    ( nextState
                        { focus = Free
                        , notListening = false
                        }
                    ) /\ [ ]
                _ ->
                    nextState /\ []
            )

        else if nextState.notListening then -- TODO: same as focus == CommandInput || focus == ValueEditor
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
                LibraryFamily idx -> nextState /\ [ SpawnNode $ FamilyIndex idx ]
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

            else

                case keyToDir kevt /\ nextState.focus of

                    Just dir /\ Node n ->
                       nextState /\ [ MoveNode (NodeIndex n) dir ]

                    _ -> nextState /\ []

            )

        else

            case Debug.spyWith "choose" show $ Either.choose (keyToDir kevt) (keyToNum kevt) of

                Just eitherNav ->
                    navigateIfNeeded eitherNav input nextState /\ []

                Nothing -> nextState /\ []


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
    NodeInlet _ _ -> false -- waits for connect or edit command
    NodeOutlet _ _ -> false -- waits for connect or edit command
    LibraryFamily _ -> false -- waits for spawn command
    Patch _ -> false -- waits for select command
    CommandInput -> false -- waits for text to be entered
    ValueEditor -> false -- waits for text to be entered



selectedNode :: State -> Maybe Int
selectedNode = _.focus >>> case _ of
    Node n -> Just n
    NodeInlets n -> Just n
    NodeOutlets n -> Just n
    NodeInlet n _  -> Just n
    NodeOutlet n _ -> Just n
    _ -> Nothing


navigateIfNeeded :: Either Dir Int -> Input -> State -> State
navigateIfNeeded (Right num) input state =
    case state.focus of
        Library         -> state { focus = LibraryFamily $ min num (input.familiesCount - 1) }
        LibraryFamily _ -> state { focus = LibraryFamily $ min num (input.familiesCount - 1) }
        PatchesBar      -> state { focus = Patch $ min num (input.patchesCount - 1) }
        Patch _         -> state { focus = Patch $ min num (input.patchesCount - 1) }
        NodesArea       -> state { focus = Node $ min num (input.nodesCount - 1) }
        Node _          -> state { focus = Node $ min num (input.nodesCount - 1) }
        NodeInlets  nodeIdx -> state { focus = NodeInlet nodeIdx  $ min num $ fromMaybe 0 $ _.inletsCount  <$> input.mbCurrentNode }
        NodeOutlets nodeIdx -> state { focus = NodeOutlet nodeIdx $ min num $ fromMaybe 0 $ _.outletsCount <$> input.mbCurrentNode }
        _ -> state
navigateIfNeeded (Left dir)  input state = state -- TODO: implement


keyToDir :: KE.KeyboardEvent -> Maybe Dir
keyToDir = KE.code >>> String.toLower >>> Debug.spy "dir" >>> case _ of
    "arrowleft" -> Just DLeft
    "arrowright" -> Just DRight
    "arrowup" -> Just DUp
    "arrowdown" -> Just DDown
    _ -> Nothing


keyToNum :: KE.KeyboardEvent -> Maybe Int
keyToNum =
    -- KE.which >>> ?wh
  KE.code >>> String.toLower >>> Debug.spy "num" >>> case _ of
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
    NodeInlet n _ -> NodeInlets n
    NodeOutlet n _ -> NodeOutlets n


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


whichToIndex :: Int -> Int -- FIXME: `kevt.which` is deprecated so there is no use for this function
whichToIndex which =
    if which >= digit0Pos && which <= digit9Pos then which - digit0Pos
    else if which >= upperAPos && which <= upperZPos then 10 + which - upperAPos
    else if which >= lowerAPos && which <= lowerZPos then 10 + which - lowerAPos
    else -1
