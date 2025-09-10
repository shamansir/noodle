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
    | OutletsOpen


type Input =
    { uiMode :: UiMode
    , nodesCount :: Int
    , familiesCount :: Int
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
    -- |
    -- |

loadNodeFocus :: Int -> Focus -> NodeFocus
loadNodeFocus nodeIdx = case _ of
    NodesArea -> Open nodeIdx
    Node n -> if n == nodeIdx then Selected else None
    _ -> None -- TODO:


trackKeyDown :: Input -> State -> KE.KeyboardEvent -> State /\ Array Action
trackKeyDown input state kevt =
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

        else if (keyCode == "n") then
            ( nextState
                { focus = NodesArea }
            ) /\ [ ]

        else

            case Either.choose (keyToDir kevt) (keyToNum kevt) of

                Just eitherNav -> navigateIfNeeded eitherNav input nextState /\ []

                Nothing -> nextState /\ []


trackKeyUp :: Input -> State -> KE.KeyboardEvent -> State /\ Array Action
trackKeyUp input state kevt = state { shiftPressed = KE.shiftKey kevt } /\ []


canZoom :: State -> Boolean
canZoom = _.shiftPressed


navigateIfNeeded :: Either Dir Int -> Input -> State -> State
navigateIfNeeded (Right num) input state =
    case state.focus of
        NodesArea -> state { focus = Node $ min num (input.nodesCount - 1) }
        Node _    -> state { focus = Node $ min num (input.nodesCount - 1) }
        _ -> state
navigateIfNeeded (Left dir)  input state = state -- TODO: implement


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
