module Web.Components.AppScreen.KeyboardLogic where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Either as Either
import Data.String as String
import Data.Tuple.Nested ((/\), type (/\))
import Web.Components.AppScreen.UiMode (UiMode(..)) as UiMode
import Web.Components.AppScreen.UiMode (UiMode)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET


data Focus
    = Free
    | Library
    | CommandInput
    | ValueEditor
    -- | LibraryItem Int
    -- | SidePanel Int
    | NodesArea
    -- | SidePanels
    | Node Int
    -- | NodeInlet Int Int
    -- | NodeOutlet Int Int


data NodeFocus
    = None
    | Selected
    | Open Int
    | InletsOpen
    | InletSelected Int
    | OutletSelected Int
    | OUtletsOpen


type State =
    { notListening :: Boolean
    , shiftPressed :: Boolean
    , focus :: Focus
    , nodesCount :: Int
    , familiesCount :: Int
    , uiMode :: UiMode
    -- , nodeStats :: Maybe { inlets :: Int, outlets :: Int}
    -- , selectedNodes :: Array Int
    }


init :: UiMode -> State
init uiMode =
    { notListening : false
    , shiftPressed : false
    , focus : Free
    , nodesCount : 0
    , familiesCount : 0
    , uiMode : uiMode
    }


data Dir
    = DLeft
    | DUp
    | DDown
    | DRight


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
    -- |
    -- |

trackKeyDown :: State -> KE.KeyboardEvent -> State /\ Array Action
trackKeyDown state kevt =
    let
        keyName = String.toLower $ KE.key kevt
        keyCode = String.toLower $ KE.code kevt
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

        else if nextState.notListening then
            nextState /\ []

        else if (keyCode == "space") then
            ( nextState
                { uiMode =
                    if not shiftPressed then
                         case state.uiMode of
                            UiMode.OnlyCanvas prev -> prev
                            other -> UiMode.OnlyCanvas other
                    else

                        case state.uiMode of
                            UiMode.SolidOverlay prev -> prev
                            other -> UiMode.SolidOverlay other
                }
            ) /\ []

        else if (keyCode == "n") then
            ( nextState
                { focus = NodesArea }
            ) /\ [ ]

        else

            case Either.choose (keyToDir kevt) (keyToNum kevt) of

                Just eitherNav -> navigateIfNeeded eitherNav nextState /\ []

                Nothing -> nextState /\ []


navigateIfNeeded :: Either Dir Int -> State -> State
navigateIfNeeded (Right num) state =
    case state.focus of
        NodesArea -> state { focus = Node $ min num (state.nodesCount - 1) }
        Node _    -> state { focus = Node $ min num (state.nodesCount - 1) }
        _ -> state
navigateIfNeeded (Left dir)  state = state


keyToDir :: KE.KeyboardEvent -> Maybe Dir
keyToDir = KE.key >>> case _ of
    "arrowleft" -> Just DLeft
    "arrowright" -> Just DRight
    "arrowup" -> Just DUp
    "arrowdown" -> Just DDown
    _ -> Nothing


keyToNum :: KE.KeyboardEvent -> Maybe Int
keyToNum = KE.key >>> case _ of
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
    _ -> Nothing


trackKeyUp :: State -> KE.KeyboardEvent -> State /\ Array Action
trackKeyUp state kevt = state { shiftPressed = KE.shiftKey kevt } /\ []


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
