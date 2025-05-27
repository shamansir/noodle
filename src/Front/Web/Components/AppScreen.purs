module Web.Components.AppScreen where

import Prelude

import Debug as Debug

import Effect.Class (liftEffect, class MonadEffect)
import Effect.Console (log) as Console

import Type.Proxy (Proxy(..))

import Control.Monad.State (get, put, modify, modify_) as State
import Control.Monad.Extra (whenJust, whenJust2, whenJust_)

import Signal ((~>))
import Signal (runSignal) as Signal

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Map (empty, toUnfoldable, size, insert, lookup) as Map
import Data.Map.Extra (update') as Map
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (unwrap) as NT
import Data.Text.Format (nil) as T
import Data.Int (round, toNumber, floor) as Int
import Data.String (toLower) as String
import Data.Array (length) as Array
import Data.Traversable (traverse_, for)
import Data.FunctorWithIndex (mapWithIndex)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Properties.Extra (Position(..), position, position_) as HHP
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.Color as HC
import Halogen.Svg.Attributes.Color.Extra as HCColorX
import Halogen.Svg.Elements as HS
import Halogen.Subscription as HSS
import Halogen.Query.Event (eventListener)

import Web.Event.Event as E
import Web.HTML (Window, window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (toEventTarget, fromEventTarget, innerWidth, innerHeight) as Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import Noodle.Wiring (class Wiring)
import Noodle.Id (PatchR, FamilyR, NodeR) as Id
import Noodle.Toolkit (Toolkit, class MarkToolkit, class HasChRepr)
import Noodle.Toolkit (families, class HoldsFamilies, class InitPatchState, class FromToPatchState, initPatch, spawnAnyRaw, loadFromPatch) as Toolkit
import Noodle.Network (toolkit, patches) as Network
import Noodle.Patch as Patch
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (run, _runOnInletUpdates, NodeChanges, id, family, state, setState, subscribeChanges, curChanges) as RawNode
import Noodle.Repr.Tagged (class ValueTagged)
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.ChRepr (class WriteChannelRepr)
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Fn.Signature (class PossiblyToSignature)
import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette
import Noodle.Ui.Tagging.At (ChannelLabel, StatusLine, Documentation) as At
import Noodle.Ui.Tagging.At (class At) as T
import Noodle.Text.NdfFile.FamilyDef.Codegen (class ParseableRepr, class ValueEncode, encodeValue) as Ndf
import Noodle.Text.NdfFile.Types (EncodedValue(..)) as Ndf
import Noodle.Text.NdfFile.Command.FromInput (CommandResult(..)) as FI
import Noodle.Text.NdfFile.Command.Quick as QOp

import Web.Components.AppScreen.State (State)
import Web.Components.AppScreen.State
    ( init, log, UiMode(..), spawnPatch, registerPatch, indexOfPatch
    , currentPatch, withCurrentPatch, replacePatch, currentPatchState', currentPatchId, currentPatchInfo
    , PatchStats, registerNewNode, updateNodePosition, nextHelpContext
    , trackCommand, trackCommandOp, switchDocumentation
    ) as CState
import Web.Components.PatchesBar as PatchesBar
import Web.Components.Library as Library
import Web.Components.PatchArea as PatchArea
import Web.Components.StatusBar as StatusBar
import Web.Components.CommandInput as CommandInput
import Web.Components.HelpText as HelpText
import Web.Components.SidePanel (SidePanel)
import Web.Components.SidePanel (panel) as SidePanel
import Web.Components.SidePanel.Console (sidePanel, panelId) as SP.ConsoleLog
import Web.Components.SidePanel.CommandLog (sidePanel, panelId) as SP.Commands
import Web.Components.SidePanel.Tree (sidePanel, panelId) as SP.Tree
import Web.Components.SidePanel.Documentation (sidePanel, panelId) as SP.Documentation
import Web.Class.WebRenderer (class WebLocator, class WebEditor)
import Web.Layer (TargetLayer(..))

import Front.Shared.Panels (Which(..)) as Panels


import HydraTk.Lang.Program (formProgram, printToJavaScript, class ToHydraCommand, collectHydraCommands) as Hydra -- FIXME
import HydraTk.Patch (resize, executeHydra) as Hydra -- FIXME


type Slots sr cr m =
    ( patchesBar :: forall q. H.Slot q PatchesBar.Output Unit
    , library :: forall q. H.Slot q Library.Output TargetLayer
    , patchArea :: H.Slot (PatchArea.Query sr cr) (PatchArea.Output sr cr) TargetLayer
    , statusBar :: H.Slot StatusBar.Query StatusBar.Output TargetLayer
    , commandInput :: forall q. H.Slot q (CommandInput.Output sr cr m) Unit
    , helpText :: forall q o. H.Slot q o Unit
    , sidePanel :: forall q o. H.Slot q o (TargetLayer /\ Panels.Which)
    )


_library = Proxy :: _ "library"
_patchesBar = Proxy :: _ "patchesBar"
_patchArea = Proxy :: _ "patchArea"
_statusBar = Proxy :: _ "statusBar"
_commandInput = Proxy :: _ "commandInput"
_helpText = Proxy :: _ "helpText"
_sidePanel = Proxy :: _ "sidePanel"


data Action sr cr m
    = Initialize
    | GlobalKeyDown KeyboardEvent
    | GlobalKeyUp KeyboardEvent
    | SelectPatch Id.PatchR
    | CreatePatch
    | SpawnNodeOf Id.FamilyR
    | RegisterNode (Raw.Node sr cr m) -- TODO: add position
    | PassUpdate Id.PatchR Id.NodeR (RawNode.NodeChanges sr cr)
    | LoadChanges Id.PatchR
    | FromPatchesBar PatchesBar.Output
    | FromLibrary Library.Output
    | FromPatchArea (Maybe Id.PatchR) (PatchArea.Output sr cr)
    | FromStatusBar StatusBar.Output
    | FromCommandInput (CommandInput.Output sr cr m)
    | HandleResize


component
    :: forall query input output loc tk ps fs sr cr m
     . Wiring m
    => WebLocator loc
    => HasFallback cr
    => HasFallback sr
    => MarkToolkit tk
    => T.At At.ChannelLabel cr
    => T.At At.StatusLine cr
    => T.At At.Documentation cr
    => Toolkit.HoldsFamilies sr cr m fs
    => Toolkit.InitPatchState tk ps m
    => Toolkit.FromToPatchState tk ps sr
    => HasChRepr tk cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => ValueTagged cr
    => Ndf.ValueEncode cr
    => Ndf.ParseableRepr cr
    => Hydra.ToHydraCommand sr
    => WebEditor tk cr m
    => Proxy loc
    -> Proxy ps
    -> Toolkit tk fs sr cr m
    -> H.Component query input output m
component ploc pps toolkit =
    H.mkComponent
        { initialState : initialState ploc toolkit
        , render : render ploc pps
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction ploc >=> const updateHelpContext
            , initialize = Just Initialize
            }
        }
    where
        updateHelpContext :: H.HalogenM (State loc tk ps fs sr cr m) (Action sr cr m) (Slots sr cr m) output m Unit
        updateHelpContext = do
            state <- H.get
            let
                mbCurrentPatch = CState.currentPatch state
                nodesCount = mbCurrentPatch <#> Patch.nodesCount # fromMaybe 0
                linksCount = mbCurrentPatch <#> Patch.linksCount # fromMaybe 0
            mbLockOn <- H.request _patchArea SVG PatchArea.QueryLock
            H.modify_ _
                { helpContext =
                    CState.nextHelpContext state
                        { nodesCount
                        , linksCount
                        , lockOn : fromMaybe PatchArea.NoLock mbLockOn
                    }
                }
            pure unit



initialState :: forall input loc tk ps fs sr cr m. Proxy loc -> Toolkit tk fs sr cr m -> input -> State loc tk ps fs sr cr m
initialState _ toolkit _ = CState.init toolkit


canvasRef :: H.RefLabel
canvasRef = H.RefLabel "target-canvas"


render
    :: forall loc tk ps fs sr cr m
     . Wiring m
    => WebLocator loc
    => Toolkit.HoldsFamilies sr cr m fs
    => Toolkit.FromToPatchState tk ps sr
    => MarkToolkit tk
    => HasFallback cr
    => HasFallback sr
    => ValueTagged cr
    => HasChRepr tk cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => Ndf.ParseableRepr cr
    => T.At At.StatusLine cr
    => T.At At.ChannelLabel cr
    => T.At At.Documentation cr
    => WebEditor tk cr m
    => Proxy loc
    -> Proxy ps
    -> State loc tk ps fs sr cr m
    -> H.ComponentHTML (Action sr cr m) (Slots sr cr m) m
render ploc _ state =
    HH.div
        [ HHP.style
            $ "background-color: " <> HC.printColor (Just solidBackground) <> ";"
            <> HHP.position_ HHP.Abs { x : 0.0, y : 0.0 }
        ]
        [ HH.canvas [ HHP.id "target-canvas", HHP.ref canvasRef, HHP.width $ Int.round width, HHP.height $ Int.round height ]
        , HH.div
            [ HHP.position HHP.Abs { x : 0.0, y : 0.0 } ]
            [ HS.svg [ HSA.width width, HSA.height height ]
                [ HS.g
                    []
                    ( case state.uiMode of
                        CState.OnlyCanvas _ ->
                            []
                        _ ->
                            [ HS.rect
                                [ HSA.width width, HSA.height height
                                , HSA.fill $ case state.uiMode of
                                    CState.OnlyCanvas _ -> Nothing -- Just <| P.hColorOf P.transparent
                                    CState.CanvasFullyVisible -> Nothing
                                    CState.TransparentOverlay opacity -> Just $ backgroundWithAlpha opacity
                                    CState.SolidOverlay _ -> Just solidBackground
                                ]
                            , HS.g
                                [ HSA.transform [ HSA.Translate 0.0 0.0 ] ]
                                [ HH.slot _patchesBar unit PatchesBar.component patchesBarInput FromPatchesBar ]
                            , HS.g
                                [ HSA.transform [ HSA.Translate libraryX libraryY ] ]
                                [ HH.slot _library SVG (Library.component ptk SVG) libraryInput FromLibrary ]
                            , HS.g
                                [ HSA.transform [ HSA.Translate patchAreaX patchAreaY ] ]
                                [ HH.slot _patchArea SVG (PatchArea.component ptk SVG) patchAreaInput $ FromPatchArea mbCurPatchId ]
                            , HS.g
                                [ HSA.transform [ HSA.Translate 0.0 statusBarY ] ]
                                [ HH.slot _statusBar SVG (StatusBar.component SVG) statusBarInput FromStatusBar ]
                            ]
                    )
                ]
            ]
        , case state.uiMode of
            CState.OnlyCanvas _ ->
                HH.div [] []
            _ ->
                HH.div_ $
                    [ HH.div
                        [ HHP.position HHP.Abs { x : statusBarX, y : statusBarY } ]
                        [ HH.slot_ _statusBar HTML (StatusBar.component HTML) statusBarInput ]
                    , HH.div
                        [ HHP.position HHP.Abs { x : libraryX, y : libraryY } ]
                        [ HH.slot _library HTML (Library.component ptk HTML) libraryInput FromLibrary ]
                    , HH.div
                        [ HHP.position HHP.Abs { x : patchAreaX, y : patchAreaY } ]
                        [ HH.slot _patchArea HTML (PatchArea.component ptk HTML) patchAreaInput $ FromPatchArea mbCurPatchId ]
                    , HH.slot _commandInput unit (CommandInput.component toolkit) commandInputInput FromCommandInput
                    ]
                    <> (mapWithIndex wrapWithPos $ panelSlot state <$> state.openPanels)
        , if state.helpText
            then HH.slot_ _helpText unit HelpText.component state.helpContext
            else HH.div [] []
        ]
        where
            toolkit = Network.toolkit state.network
            solidBackground = P.hColorOf Palette.black
            backgroundWithAlpha bgOpacity = fromMaybe solidBackground $ HCColorX.setAlpha bgOpacity solidBackground
            width  = fromMaybe 1000.0 $ _.width  <$> state.size
            height = fromMaybe 1000.0 $ _.height <$> state.size
            (ptk :: _ tk) = Proxy
            mbCurrentPatch     = CState.currentPatch state
            mbCurrentPatchInfo = CState.currentPatchInfo state
            mbCurPatchId  = mbCurrentPatch <#> Patch.id
            curPatchNodes = mbCurrentPatch <#> Patch.allNodes # fromMaybe []
            curPatchLinks = mbCurrentPatch <#> Patch.links # fromMaybe []
            curPatchNodesBounds = mbCurrentPatchInfo <#> _.nodesBounds # fromMaybe Map.empty
            curPatchState = CState.currentPatchState' state

            patchAreaX = Library.width + 20.0
            patchAreaY = PatchesBar.height + 15.0
            libraryX = 5.0
            libraryY = PatchesBar.height + 15.0
            statusBarX = 0.0
            statusBarY = height - StatusBar.height - 10.0
            patchAreaHeight = height - PatchesBar.height - 15.0 - StatusBar.height - 10.0
            patchAreaWidth = width - Library.width - 20.0
            statusBarWidth = width * 0.99
            panelsWidth = 350.0

            libraryInput =
                { families : Toolkit.families toolkit
                } :: Library.Input
            patchAreaInput =
                { offset : { left : patchAreaX, top : patchAreaY }
                , size : { width : patchAreaWidth, height : patchAreaHeight }
                , zoom : state.zoom
                , bgOpacity : 0.0 -- FIXME: state.bgOpacity
                , mbState : curPatchState
                , nodes : Debug.spy "nodes" curPatchNodes
                , nodesBounds : Debug.spy "nodesBounds" curPatchNodesBounds
                , links : curPatchLinks
                , mbCurrentEditor : state.mbCurrentEditor
                } :: PatchArea.Input ps sr cr m
            patchesBarInput =
                { patches : map Patch.name <$> (Map.toUnfoldable $ Network.patches state.network)
                , selected : _.id <$> state.mbCurrentPatch
                } :: PatchesBar.Input
            statusBarInput =
                { content : fromMaybe T.nil state.mbStatusBarContent
                , width : statusBarWidth
                , currentZoom : state.zoom
                } :: StatusBar.Input
            commandInputInput =
                { pos : { x : width / 2.0, y : height / 2.0 }
                , active : state.commandInputActive
                } :: CommandInput.Input

            panelsCount = Array.length state.openPanels
            wrapWithPos panelIdx content =
                HH.div
                    [ HHP.style $
                        HHP.position_ HHP.Abs
                            { x : width - panelsWidth
                            , y : (Int.toNumber panelIdx / Int.toNumber panelsCount) * height
                            }
                        <> " min-width: " <> show panelsWidth <> "px;"
                    ]
                    $ pure content


panelSlot
    :: forall loc tk ps fs sr cr m
     . MarkToolkit tk
    => HasChRepr tk cr
    => T.At At.Documentation cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => MonadEffect m
    => State loc tk ps fs sr cr m
    -> Panels.Which
    -> H.ComponentHTML (Action sr cr m) (Slots sr cr m) m
panelSlot state Panels.Console       = HH.slot_ _sidePanel (HTML /\ Panels.Console) (SidePanel.panel SP.ConsoleLog.panelId SP.ConsoleLog.sidePanel) state.log
panelSlot state Panels.Commands      = HH.slot_ _sidePanel (HTML /\ Panels.Commands) (SidePanel.panel SP.Commands.panelId SP.Commands.sidePanel) state.history
panelSlot state Panels.Tree          = HH.slot_ _sidePanel (HTML /\ Panels.Tree) (SidePanel.panel SP.Tree.panelId SP.Tree.sidePanel) state.network
panelSlot state Panels.Documentation = HH.slot_ _sidePanel (HTML /\ Panels.Documentation) (SidePanel.panel SP.Documentation.panelId SP.Documentation.sidePanel) state
panelSlot state Panels.WsServer      = HH.div [] []
panelSlot state Panels.HydraCode     = HH.div [] []


handleAction
    :: forall output loc tk ps fs sr cr m
     . Wiring m
    => WebLocator loc
    => Toolkit.InitPatchState tk ps m
    => Toolkit.FromToPatchState tk ps sr
    => Hydra.ToHydraCommand sr
    => HasFallback cr
    => ValueTagged cr
    => Ndf.ValueEncode cr
    => Proxy loc
    -> Action sr cr m
    -> H.HalogenM (State loc tk ps fs sr cr m) (Action sr cr m) (Slots sr cr m) output m Unit
handleAction ploc = case _ of
    Initialize -> do
        window <- H.liftEffect $ Web.window

        H.subscribe' \_ ->
            eventListener
                (E.EventType "resize")
                (Window.toEventTarget window)
                (E.target >=> (const $ Just HandleResize))

        H.subscribe' \_ ->
            eventListener
                (KET.keydown)
                (Window.toEventTarget window)
                (KE.fromEvent >=> (Just <<< GlobalKeyDown))

        H.subscribe' \_ ->
            eventListener
                (KET.keyup)
                (Window.toEventTarget window)
                (KE.fromEvent >=> (Just <<< GlobalKeyUp))

        {- H.getHTMLElementRef canvasRef >>= traverse_ \element -> do
            ?wh -- TODO
        -}

        state <- H.get
        firstPatch <- H.lift $ CState.spawnPatch state
        H.modify_ $ CState.registerPatch firstPatch.state firstPatch.patch

        H.modify_ $ CState.log "Initialize"

        handleAction ploc HandleResize
    HandleResize -> do
        window    <- H.liftEffect $ Web.window
        newWidth  <- H.liftEffect $ Window.innerWidth window
        newHeight <- H.liftEffect $ Window.innerHeight window
        H.modify_ $ _ { size = Just { width : Int.toNumber newWidth, height : Int.toNumber newHeight } }
        H.liftEffect $ Hydra.resize newWidth newHeight
        pure unit
    CreatePatch -> do
        state <- H.get
        newPatch <- H.lift $ CState.spawnPatch state
        H.modify_ $ CState.registerPatch newPatch.state newPatch.patch
        handleAction ploc $ FromPatchesBar $ PatchesBar.SelectPatch $ Patch.id newPatch.patch
    SelectPatch patchR -> do
        state <- H.get
        H.modify_ _
            { mbCurrentPatch =
                CState.indexOfPatch patchR state
                    <#> (\pIndex -> { id : patchR, index : pIndex })
            }
    SpawnNodeOf familyR -> do
        state <- H.get
        let toolkit = Network.toolkit state.network
        (mbRawNode :: Maybe (Raw.Node sr cr m)) <- H.lift $ Toolkit.spawnAnyRaw familyR toolkit
        whenJust mbRawNode (handleAction ploc <<< RegisterNode)
    RegisterNode rawNode -> do
        let
            familyR = RawNode.family rawNode
            nodeR = RawNode.id rawNode
        H.lift $ RawNode._runOnInletUpdates rawNode

        mbCurrentPatch <- CState.currentPatch <$> H.get
        whenJust mbCurrentPatch \curPatch -> do
            (patchState :: ps) <- Patch.getState curPatch
            (curState :: sr) <- RawNode.state rawNode
            let (mbNodeState :: Maybe sr) = Toolkit.loadFromPatch (Proxy :: _ tk) familyR patchState curState
            -- whenJust (mbNodeState >>= StRepr.from) $ flip Node.setState node
            whenJust mbNodeState
                \nextState -> rawNode # RawNode.setState nextState

            _ <- H.subscribe =<< do -- TODO: make one emitter for all nodes to be a bus with all the changes in the patch
                { emitter, listener } <- H.liftEffect HSS.create
                H.liftEffect
                    $  Signal.runSignal
                    $  RawNode.subscribeChanges rawNode
                    ~> PassUpdate (Patch.id curPatch) nodeR
                    ~> HSS.notify listener
                pure emitter

            H.modify_
                  $ CState.registerNewNode (Patch.id curPatch) rawNode
                >>> CState.trackCommandOp (QOp.makeNode nodeR { left: 0, top : 0 })

            Patch.trackStateChangesFromRaw (Proxy :: _ tk) rawNode curPatch

            H.lift $ RawNode.run rawNode
    PassUpdate patchR nodeR update ->
        H.get >>= CState.currentPatch >>> whenJust_ \curPatch -> do
            when (Patch.id curPatch == patchR) $
                H.tell _patchArea SVG $ PatchArea.ApplyUpdate nodeR update
            collectedCommands <- H.lift $ Hydra.collectHydraCommands curPatch
            when (Map.size collectedCommands > 0) $ H.liftEffect $ do
                let program = Hydra.printToJavaScript $ Hydra.formProgram collectedCommands
                Hydra.executeHydra program
                Console.log program
    LoadChanges patchR -> do
        mbCurrentPatch <- CState.currentPatch <$> H.get
        whenJust mbCurrentPatch \curPatch -> do
            void $ for (Patch.allNodes curPatch) $ \rawNode -> do
                curChanges <- RawNode.curChanges rawNode
                let nodeR = RawNode.id rawNode
                H.tell _patchArea SVG $ PatchArea.ApplyUpdate nodeR curChanges
    FromPatchesBar (PatchesBar.SelectPatch patchR) -> do
        handleAction ploc $ SelectPatch patchR
    FromPatchesBar PatchesBar.CreatePatch -> do
        handleAction ploc $ CreatePatch
    FromLibrary (Library.SelectFamily familyR) ->
        handleAction ploc $ SpawnNodeOf familyR
    FromPatchArea Nothing _ -> do
        pure unit
    FromPatchArea _ (PatchArea.TryZoom dy) -> do
        state <- H.get
        when state.shiftPressed $
            H.put $ state { zoom = min 3.0 $ max 0.3 $ state.zoom + (dy * 0.1) }
    FromPatchArea _ (PatchArea.Connect (source /\ target)) -> do
        mbCurrentPatch <- CState.currentPatch <$> H.get
        whenJust mbCurrentPatch \curPatch -> do
            whenJust2 (Patch.findRawNode source.fromNode curPatch) (Patch.findRawNode target.toNode curPatch)
                \srcNode dstNode -> do
                    nextPatch /\ rawLink <-
                        H.lift $ Patch.connectRaw
                            source.fromOutlet
                            target.toInlet
                            srcNode
                            dstNode
                            curPatch
                    H.modify_
                          $ CState.replacePatch (Patch.id curPatch) nextPatch
                        >>> CState.trackCommandOp (QOp.connect rawLink)
    FromPatchArea _ (PatchArea.Disconnect linkR) -> do
        mbCurrentPatch <- CState.currentPatch <$> H.get
        whenJust mbCurrentPatch \curPatch -> do
            whenJust (Patch.findRawLink linkR curPatch) \rawLink -> do
                nextPatch /\ _ <- H.lift $ Patch.disconnectRaw rawLink curPatch
                H.modify_
                      $ CState.replacePatch (Patch.id curPatch) nextPatch
                    >>> CState.trackCommandOp (QOp.disconnect rawLink)
    FromPatchArea _ (PatchArea.UpdateStatusBar tag) ->
        H.modify_ _ { mbStatusBarContent = Just tag }
    FromPatchArea _ PatchArea.ClearStatusBar ->
        H.modify_ _ { mbStatusBarContent = Nothing }
    FromPatchArea _ (PatchArea.RemoveNode nodeR) -> do
        mbCurrentPatch <- CState.currentPatch <$> H.get
        whenJust mbCurrentPatch \curPatch -> do
            nextCurrentPatch <- H.lift $ Patch.disconnectAllFromTo nodeR curPatch
            H.modify_
                   $ CState.replacePatch (Patch.id curPatch) (nextCurrentPatch # Patch.removeNode nodeR)
                >>> CState.trackCommandOp (QOp.removeNode nodeR)
    FromPatchArea _ (PatchArea.RequestValueEditor nodeR valueEditor) -> do
        H.modify_ _ { mbCurrentEditor = Just $ nodeR /\ valueEditor }
    FromPatchArea (Just patchR) (PatchArea.MoveNode nodeR pos) -> do
        H.modify_
              $ CState.updateNodePosition patchR nodeR pos
            >>> CState.trackCommandOp
                    (QOp.moveNode nodeR
                        { left : Int.floor pos.left
                        , top  : Int.floor pos.top
                        }
                    )
    FromPatchArea _ PatchArea.CloseValueEditor ->
        H.modify_ $ _ { mbCurrentEditor = Nothing }
    FromPatchArea (Just patchR) (PatchArea.TrackValueSend nodeR inletR value) -> do
        H.modify_ $ CState.trackCommandOp $ QOp.sendIn nodeR inletR $ fromMaybe (Ndf.EncodedValue "?") $ Ndf.encodeValue value
    FromPatchArea _ PatchArea.RefreshHelp ->
        pure unit -- Help is refreshed on every `handleAction` cycle above
    FromPatchArea _ (PatchArea.RequestDocumentation focus) ->
        H.modify_ $ CState.switchDocumentation focus.node focus.curUpdate
    FromStatusBar StatusBar.ResetZoom ->
        H.modify_ $ _ { zoom = 1.0 }
    FromCommandInput (CommandInput.ExecuteCommand cmdResult) ->
        case cmdResult of
            FI.FromFamily familyR rawNode ->
                handleAction ploc $ RegisterNode rawNode
            FI.CustomNode signature rawNode ->
                handleAction ploc $ RegisterNode rawNode
            FI.CannotSpawn familyR ->
                pure unit
                -- FIXME: log error: CC.log $ "family not found: " <> Id.family familyR
            FI.UnknownCommand command ->
                pure unit
                -- FIXME: log error: CC.log $ "parse error " <> command
    FromCommandInput CommandInput.CloseCommandInput ->
        H.modify_ _ { commandInputActive = false }
    GlobalKeyDown kevt -> do
        let keyName = Debug.spy "keyname" $ String.toLower $ KE.key kevt
        let keyCode = Debug.spy "keycode" $ String.toLower $ KE.code kevt
        let shiftPressed = Debug.spy "shiftPressed" $ KE.shiftKey kevt
        let controlPressed = Debug.spy "controlPressed" $ KE.ctrlKey kevt
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
            mbCurPatchId <- CState.currentPatchId <$> H.get
            whenJust mbCurPatchId \curPatchId -> do
                nextMode <- _.uiMode <$> H.get
                case nextMode of
                    CState.OnlyCanvas _ -> pure unit
                    _ -> handleAction ploc $ LoadChanges curPatchId
        when ((keyName == "h") && controlPressed) $ do
            H.modify_ \s -> s { helpText = not s.helpText }
    GlobalKeyUp kevt ->
        H.modify_ $ _ { shiftPressed = KE.shiftKey kevt }
