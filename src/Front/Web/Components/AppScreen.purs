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
import Data.Map (empty, toUnfoldable, fromFoldable, size, insert, lookup) as Map
import Data.Map.Extra (update') as Map
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (unwrap) as NT
import Data.Text.Format (nil) as T
import Data.Int (round, toNumber, floor) as Int
import Data.String (toLower) as String
import Data.Array (length) as Array
import Data.Set (size, insert, delete, toUnfoldable) as Set
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
import Noodle.Text.NdfFile.Types (EncodedValue(..), Coord(..)) as Ndf
import Noodle.Text.NdfFile.Command.FromInput (CommandResult(..)) as FI
import Noodle.Text.NdfFile.Command.Quick as QOp
import Noodle.Text.NdfFile.Command.Op (CommandOp(..)) as Ndf

import Web.Components.AppScreen.State (State)
import Web.Components.AppScreen.State as CState
import Web.Components.PatchesBar as PatchesBar
import Web.Components.Library as Library
import Web.Components.PatchArea as PatchArea
import Web.Components.StatusBar as StatusBar
import Web.Components.CommandInput as CommandInput
import Web.Components.HelpText as HelpText
import Web.Components.PanelTogglesBar as PanelTogglesBar
import Web.Components.SidePanel (SidePanel)
import Web.Components.SidePanel (panel, charOf, RenderParams) as SidePanel
import Web.Components.SidePanel.Console (sidePanel, panelId) as SP.ConsoleLog
import Web.Components.SidePanel.CommandLog (sidePanel, panelId) as SP.Commands
import Web.Components.SidePanel.Tree (sidePanel, panelId) as SP.Tree
import Web.Components.SidePanel.Documentation (sidePanel, panelId) as SP.Documentation
import Web.Components.SidePanel.WebSocketStatus (sidePanel, panelId) as SP.WSStatus
import Web.Components.SidePanel.HydraCode (sidePanel, panelId) as SP.HydraCode
import Web.Class.WebRenderer (class WebLocator, class WebEditor)
import Web.Layer (TargetLayer(..))
import Web.Layouts (noodleUI, UiParams) as Layouts
import Web.Layouts (UiPart(..)) as Ui

import Play as Play

import Front.Shared.Bounds (toNumberPosition) as Bounds
import Front.Shared.Panels (Which(..), allPanels) as Panels
import Front.Shared.WsLocation (host, port) as WSLoc
import Front.Shared.StatusBarCells as SPCells

import HydraTk.Lang.Program (formProgram, printToJavaScript, class ToHydraCommand, collectHydraCommands) as Hydra -- FIXME
import HydraTk.Patch (resize, executeHydra) as Hydra -- FIXME

import WebSocket.Types (WebSocket)
import WebSocket.Client.Socket (handleEv, createWebSocket, Event(..)) as WSocket
import Noodle.Text.WsMessage (ndfOp) as WSMsg


type Slots sr cr m =
    ( patchesBar :: forall q. H.Slot q PatchesBar.Output Unit
    , library :: forall q. H.Slot q Library.Output TargetLayer
    , patchArea :: H.Slot (PatchArea.Query sr cr) (PatchArea.Output sr cr) TargetLayer
    , statusBar :: H.Slot StatusBar.Query StatusBar.Output TargetLayer
    , commandInput :: forall q. H.Slot q (CommandInput.Output sr cr m) Unit
    , helpText :: forall q o. H.Slot q o Unit
    , panelToggles :: forall q. H.Slot q PanelTogglesBar.Output Unit
    , sidePanel :: forall q o. H.Slot q o (TargetLayer /\ Panels.Which)
    , statusBarCell :: StatusBar.CellSlot
    )


_library = Proxy :: _ "library"
_patchesBar = Proxy :: _ "patchesBar"
_patchArea = Proxy :: _ "patchArea"
_statusBar = Proxy :: _ "statusBar"
_statusBarCell = Proxy :: _ "statusBarCell"
_commandInput = Proxy :: _ "commandInput"
_helpText = Proxy :: _ "helpText"
_panelToggles = Proxy :: _ "panelToggles"
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
    | LoadCurrentPatchChanges
    | FromPatchesBar PatchesBar.Output
    | FromLibrary Library.Output
    | FromPatchArea (Maybe Id.PatchR) (PatchArea.Output sr cr)
    | FromStatusBar StatusBar.Output
    | FromCommandInput (CommandInput.Output sr cr m)
    | FromPanelToggles PanelTogglesBar.Output
    | FromWebSocket WebSocket WSocket.Event
    | FromStatusBarCell SPCells.Which SPCells.Output
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
                            uiLayoutItems <#> \{ rect, v } ->

                                case v of
                                    Ui.Background ->
                                        HS.rect
                                            [ HSA.width rect.size.width, HSA.height rect.size.height
                                            , HSA.fill $ case state.uiMode of
                                                CState.OnlyCanvas _ -> Nothing -- Just <| P.hColorOf P.transparent
                                                CState.CanvasFullyVisible -> Nothing
                                                CState.TransparentOverlay opacity -> Just $ backgroundWithAlpha opacity
                                                CState.SolidOverlay _ -> Just solidBackground
                                            ]

                                    Ui.PatchesBar ->
                                        HS.g
                                            [ HSA.transform [ HSA.Translate rect.pos.x rect.pos.y ] ]
                                            [ HH.slot _patchesBar unit PatchesBar.component patchesBarInput FromPatchesBar ]

                                    Ui.SidePanelsButtons ->
                                        HS.g
                                            [ HSA.transform [ HSA.Translate rect.pos.x rect.pos.y ] ]
                                            [ HH.slot _panelToggles unit PanelTogglesBar.component panelToggleInput FromPanelToggles ]

                                    Ui.Library ->
                                        HS.g
                                            [ HSA.transform [ HSA.Translate rect.pos.x rect.pos.y ] ]
                                            [ HH.slot _library SVG (Library.component ptk SVG) libraryInput FromLibrary ]

                                    Ui.Nodes ->
                                        HS.g
                                            [ HSA.transform [ HSA.Translate rect.pos.x rect.pos.y ] ]
                                            [ HH.slot _patchArea SVG (PatchArea.component ptk SVG) patchAreaInput $ FromPatchArea mbCurPatchId ]

                                    Ui.StatusBar ->
                                        HS.g
                                            [ HSA.transform [ HSA.Translate rect.pos.x rect.pos.y ] ]
                                            [ HH.slot _statusBar SVG (StatusBar.component SVG) statusBarInput FromStatusBar ]

                                    Ui.SidePanel n which ->
                                        HS.g
                                            [ HSA.transform [ HSA.Translate rect.pos.x rect.pos.y ] ]
                                            [ panelSlot sidePanelParams SVG state which ]

                                    Ui.StatusBarSection n which ->
                                        HS.g
                                            [ HSA.transform [ HSA.Translate rect.pos.x rect.pos.y ] ]
                                            [ StatusBar.cellSlot _statusBarCell SVG (FromStatusBarCell which) statusBarInput which ]

                                    _ -> HH.text "" -- HS.g [] []

                            {-
                            [ HS.rect
                                [ HSA.width width, HSA.height height
                                , HSA.fill $ case state.uiMode of
                                    CState.OnlyCanvas _ -> Nothing -- Just <| P.hColorOf P.transparent
                                    CState.CanvasFullyVisible -> Nothing
                                    CState.TransparentOverlay opacity -> Just $ backgroundWithAlpha opacity
                                    CState.SolidOverlay _ -> Just solidBackground
                                ]
                            , HS.g
                                [ HSA.transform [ HSA.Translate patchesBarX patchesBarY ] ]
                                [ HH.slot _patchesBar unit PatchesBar.component patchesBarInput FromPatchesBar ]
                            , HS.g
                                [ HSA.transform [ HSA.Translate panelTogglesX panelTogglesY ] ]
                                [ HH.slot _panelToggles unit PanelTogglesBar.component panelToggleInput FromPanelToggles ]
                            , HS.g
                                [ HSA.transform [ HSA.Translate libraryX libraryY ] ]
                                [ HH.slot _library SVG (Library.component ptk SVG) libraryInput FromLibrary ]
                            , HS.g
                                [ HSA.transform [ HSA.Translate patchAreaX patchAreaY ] ]
                                [ HH.slot _patchArea SVG (PatchArea.component ptk SVG) patchAreaInput $ FromPatchArea mbCurPatchId ]
                            , HS.g
                                [ HSA.transform [ HSA.Translate statusBarX statusBarY ] ]
                                [ HH.slot _statusBar SVG (StatusBar.component SVG) statusBarInput FromStatusBar ]
                            ] <> (mapWithIndex wrapSvgWithPos $ panelSlot sidePanelParams SVG state <$> Set.toUnfoldable state.openPanels)
                            -}
                    )
                ]
            ]
        , case state.uiMode of
            CState.OnlyCanvas _ ->
                HH.div [] []
            _ ->
                HH.div_ $
                    ( uiLayoutItems <#> \{ rect, v } ->

                        case v of

                            Ui.Library ->
                                HH.div
                                    [ HHP.position HHP.Abs { x : rect.pos.x, y : rect.pos.y } ]
                                    [ HH.slot _library HTML (Library.component ptk HTML) libraryInput FromLibrary ]

                            Ui.Nodes ->
                                HH.div
                                    [ HHP.position HHP.Abs { x : rect.pos.x, y : rect.pos.y } ]
                                    [ HH.slot _patchArea HTML (PatchArea.component ptk HTML) patchAreaInput $ FromPatchArea mbCurPatchId ]

                            Ui.StatusBar ->
                                HH.div
                                    [ HHP.position HHP.Abs { x : rect.pos.x, y : rect.pos.y } ]
                                    [ HH.slot_ _statusBar HTML (StatusBar.component HTML) statusBarInput ]

                            Ui.SidePanel n which ->
                                HH.div
                                    [ HHP.position HHP.Abs { x : rect.pos.x, y : rect.pos.y } ]
                                    [ panelSlot sidePanelParams HTML state which ]

                            Ui.StatusBarSection n which ->
                                HS.g
                                    [ HSA.transform [ HSA.Translate rect.pos.x rect.pos.y ] ]
                                    [ StatusBar.cellSlot _statusBarCell HTML (FromStatusBarCell which) statusBarInput which ]

                            _ -> HH.div [] []
                    )

                    <> [ HH.slot _commandInput unit (CommandInput.component toolkit) commandInputInput FromCommandInput ]

                    {-
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
                    <> (mapWithIndex wrapHtmlWithPos $ panelSlot sidePanelParams HTML state <$> Set.toUnfoldable state.openPanels)
                    -}
        , if state.helpText
            then HH.slot_ _helpText unit HelpText.component state.helpContext
            else HH.div [] []
        ]
        where
            defaultSize = { width : 1000.0, height : 1000.0 }
            layoutParams =
              { size : fromMaybe defaultSize state.size
              , sidePanels : state.openPanels
              , statusBarSections : 3
              }
            uiLayout = Play.layout $ Layouts.noodleUI layoutParams
            uiLayoutItems = Play.flattenLayout uiLayout

            -- nodeLayout nodeParams = Layouts.horzNodeUI nodeParams

            -- layout = Play.layout $ Layout.noodleUI
            -- layoutItems = Play.flattenLayout layout

            toolkit = Network.toolkit state.network
            solidBackground = P.hColorOf Palette.black
            backgroundWithAlpha bgOpacity = fromMaybe solidBackground $ HCColorX.setAlpha bgOpacity solidBackground
            width  = fromMaybe defaultSize.width  $ _.width  <$> state.size
            height = fromMaybe defaultSize.height $ _.height <$> state.size
            (ptk :: _ tk) = Proxy
            mbCurrentPatch     = CState.currentPatch state
            mbCurrentPatchInfo = CState.currentPatchInfo state
            mbCurPatchId  = mbCurrentPatch <#> Patch.id
            curPatchNodes = mbCurrentPatch <#> Patch.allNodes # fromMaybe []
            curPatchLinks = mbCurrentPatch <#> Patch.links # fromMaybe []
            curPatchNodesBounds = mbCurrentPatchInfo <#> _.nodesBounds # fromMaybe Map.empty
            curPatchState = CState.currentPatchState' state

            patchesBarX = 0.0
            patchesBarY = 0.0
            patchAreaX = Library.width + 20.0
            patchAreaY = PatchesBar.height + 15.0
            libraryX = 5.0
            libraryY = PatchesBar.height + 15.0
            statusBarX = 0.0
            statusBarY = height - StatusBar.height - 10.0
            panelTogglesX = width - PanelTogglesBar.width
            panelTogglesY = 0.0
            patchAreaHeight = height - PatchesBar.height - 15.0 - StatusBar.height - 10.0
            patchAreaWidth = width - Library.width - 20.0
            statusBarWidth = width * 0.99
            sidePanelWidth = 350.0
            sidePanelHeight = sidePanelsHeight / Int.toNumber panelsCount
            sidePanelsX = width - sidePanelWidth
            sidePanelsY = PatchesBar.height + 15.0
            sidePanelsHeight = height - sidePanelsY - StatusBar.height - 20.0

            libraryInput =
                { families : Toolkit.families toolkit
                } :: Library.Input
            patchAreaInput =
                { offset : { left : patchAreaX, top : patchAreaY }
                , size : { width : patchAreaWidth, height : patchAreaHeight }
                , zoom : state.zoom
                , bgOpacity : 0.0 -- FIXME: state.bgOpacity
                , mbState : curPatchState
                , nodes : curPatchNodes
                , nodesBounds : curPatchNodesBounds
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
                , wsStatus : state.wsConnection.status
                } :: StatusBar.Input
            commandInputInput =
                { pos : { x : width / 2.0, y : height / 2.0 }
                , active : state.commandInputActive
                } :: CommandInput.Input
            panelToggleInput =
                { openPanels : state.openPanels
                , symbols : collectedSymbols
                } :: PanelTogglesBar.Input

            sidePanelParams =
                { size : { width : sidePanelWidth, height : sidePanelHeight }
                } :: SidePanel.RenderParams

            collectedSymbols = Map.fromFoldable $ (/\) <*> panelSymbol state <$> Panels.allPanels
            panelsCount = Set.size state.openPanels
            sidePanelY panelIdx = (Int.toNumber panelIdx / Int.toNumber panelsCount) * sidePanelsHeight
            wrapSvgWithPos panelIdx content =
                HS.g
                    [ HSA.transform [ HSA.Translate sidePanelsX $ sidePanelsY + sidePanelY panelIdx ]
                    ]
                    $ pure content
            wrapHtmlWithPos panelIdx content =
                HH.div
                    [ HHP.style $
                        HHP.position_ HHP.Abs
                            { x : sidePanelsX
                            , y : sidePanelsY + sidePanelY panelIdx
                            }
                        <> " min-width: " <> show sidePanelWidth <> "px;"
                    ]
                    $ pure content


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

        ws <- H.liftEffect $ HSS.create
        wSocket <- H.liftEffect $ WSocket.createWebSocket WSLoc.host WSLoc.port []
        _ <- H.subscribe $ FromWebSocket wSocket <$> ws.emitter
        H.modify_ $ CState.markWSWaiting wSocket
        liftEffect $ WSocket.handleEv (HSS.notify ws.listener) wSocket

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
        handleAction ploc LoadCurrentPatchChanges
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

            let moveCommandOp = QOp.makeNode nodeR { left: 0, top : 0 }

            H.modify_
                  $ CState.registerNewNode (Patch.id curPatch) rawNode
                >>> CState.trackCommandOp moveCommandOp

            sendNdfOpToWebSocket moveCommandOp

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
    LoadCurrentPatchChanges -> do
        mbCurrentPatch <- CState.currentPatch <$> H.get
        whenJust mbCurrentPatch \curPatch -> do
            void $ for (Patch.allNodes curPatch) $ \rawNode -> do
                curChanges <- RawNode.curChanges rawNode
                let nodeR = RawNode.id rawNode
                H.tell _patchArea SVG $ PatchArea.ApplyUpdate nodeR curChanges

    {- FromPatchArea -}

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
                    let connectCommandOp = QOp.connect rawLink
                    H.modify_
                          $ CState.replacePatch (Patch.id curPatch) nextPatch
                        >>> CState.trackCommandOp connectCommandOp
                    sendNdfOpToWebSocket connectCommandOp
    FromPatchArea _ (PatchArea.Disconnect linkR) -> do
        mbCurrentPatch <- CState.currentPatch <$> H.get
        whenJust mbCurrentPatch \curPatch -> do
            whenJust (Patch.findRawLink linkR curPatch) \rawLink -> do
                nextPatch /\ _ <- H.lift $ Patch.disconnectRaw rawLink curPatch
                let disconnectCommandOp = QOp.disconnect rawLink
                H.modify_
                      $ CState.replacePatch (Patch.id curPatch) nextPatch
                    >>> CState.trackCommandOp disconnectCommandOp
                sendNdfOpToWebSocket disconnectCommandOp
    FromPatchArea _ (PatchArea.UpdateStatusBar tag) ->
        H.modify_ _ { mbStatusBarContent = Just tag }
    FromPatchArea _ PatchArea.ClearStatusBar ->
        H.modify_ _ { mbStatusBarContent = Nothing }
    FromPatchArea _ (PatchArea.RemoveNode nodeR) -> do
        mbCurrentPatch <- CState.currentPatch <$> H.get
        whenJust mbCurrentPatch \curPatch -> do
            nextCurrentPatch <- H.lift $ Patch.disconnectAllFromTo nodeR curPatch
            let removeNodeCommandOp = QOp.removeNode nodeR
            H.modify_
                   $ CState.replacePatch (Patch.id curPatch) (nextCurrentPatch # Patch.removeNode nodeR)
                >>> CState.trackCommandOp removeNodeCommandOp
            sendNdfOpToWebSocket removeNodeCommandOp
    FromPatchArea _ (PatchArea.RequestValueEditor nodeR valueEditor) -> do
        H.modify_ _ { mbCurrentEditor = Just $ nodeR /\ valueEditor }
    FromPatchArea (Just patchR) (PatchArea.MoveNode nodeR pos) -> do
        let moveNodeCommandOp =
                QOp.moveNode nodeR
                        { left : Int.floor pos.left
                        , top  : Int.floor pos.top
                        }
        H.modify_
              $ CState.updateNodePosition patchR nodeR pos
            >>> CState.trackCommandOp moveNodeCommandOp
        sendNdfOpToWebSocket moveNodeCommandOp
    FromPatchArea _ PatchArea.CloseValueEditor ->
        H.modify_ $ _ { mbCurrentEditor = Nothing }
    FromPatchArea (Just patchR) (PatchArea.TrackValueSend nodeR inletR value) -> do
        let sendInCommandOp = QOp.sendIn nodeR inletR $ fromMaybe (Ndf.EncodedValue "?") $ Ndf.encodeValue value
        H.modify_ $ CState.trackCommandOp sendInCommandOp
        sendNdfOpToWebSocket sendInCommandOp
    FromPatchArea _ PatchArea.RefreshHelp ->
        pure unit -- Help is refreshed on every `handleAction` cycle above
    FromPatchArea _ (PatchArea.RequestDocumentation focus) ->
        H.modify_ $ CState.switchDocumentation focus.node focus.curUpdate

    {- FromStatusBar -}

    FromStatusBar StatusBar.ResetZoom ->
        H.modify_ $ _ { zoom = 1.0 }

    FromStatusBarCell _ _ ->
        pure unit -- FIXME
        -- H.modify_ $ _ { zoom = 1.0 }

    {- FromCommandInput -}

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

    {- FromPanelToggles -}

    FromPanelToggles (PanelTogglesBar.OpenPanel which) ->
        H.modify_ \s -> s { openPanels = Set.insert which s.openPanels }
    FromPanelToggles (PanelTogglesBar.ClosePanel which) ->
        H.modify_ \s -> s { openPanels = Set.delete which s.openPanels }

    {- FromWebSocket -}

    FromWebSocket wSocket WSocket.Open ->
        H.modify_ $ CState.markWSConnected wSocket
    FromWebSocket wSocket (WSocket.Close closeCode closeReason) ->
        H.modify_ $ CState.markWSDisconnect wSocket
    FromWebSocket wSocket (WSocket.Message wsMessage) ->
        H.modify_ $ CState.storeWSNativeMessage wsMessage
    FromWebSocket wSocket (WSocket.Error error) ->
        H.modify_ $ CState.markWSError wSocket

    {- GlobalKeyDown -}

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


sendNdfOpToWebSocket :: forall loc tk ps fs sr cr m action slots output. MonadEffect m => Ndf.CommandOp -> H.HalogenM (State loc tk ps fs sr cr m) action slots output m Unit
sendNdfOpToWebSocket ndfOp = H.get >>= H.lift <<< CState.sendWSMessage (WSMsg.ndfOp ndfOp)


-- FIXME: many commands handing here is very similar to handling `FromPatchArea` or some other actions in `handleAction`
--        the difference is that they operate `Ndf.InstanceId` and `Ndf.EncodedValue` and others, and they assume current patch as where all the actions are performed
--        we found the generalize them in `Front.Cli.Actions`, so may be here is also can be done.
--        find some way to better generalize them
applyCommand :: forall loc tk ps fs sr cr m output
     . Wiring m
    => WebLocator loc
    => Toolkit.InitPatchState tk ps m
    => Toolkit.FromToPatchState tk ps sr
    => Hydra.ToHydraCommand sr
    => HasFallback cr
    => ValueTagged cr
    => Ndf.ValueEncode cr
    => Proxy loc
    -> Ndf.CommandOp
    -> H.HalogenM (State loc tk ps fs sr cr m) (Action sr cr m) (Slots sr cr m) output m Unit
applyCommand ploc = case _ of -- FIXME: implement
    Ndf.DefineFamily familyDef -> pure unit
    Ndf.AssignProcess processFn -> pure unit
    Ndf.MakeNode familyR coordX coordY instanceId -> do
        -- FIXME: almost the same as: `handleAction ploc $ SpawnNodeOf familyR`, we just need Id.NodeR from new node
        state <- H.get
        let toolkit = Network.toolkit state.network
        (mbRawNode :: Maybe (Raw.Node sr cr m)) <- H.lift $ Toolkit.spawnAnyRaw familyR toolkit
        whenJust mbRawNode $ \rawNode -> do
            handleAction ploc $ RegisterNode rawNode
            H.modify_ $ CState.rememberNdfInstance (RawNode.id rawNode) instanceId
    Ndf.Move instanceId (Ndf.Coord coordX) (Ndf.Coord coordY) -> do
        state <- H.get
        let
            mbNodeR = CState.findIdForNdfInstance instanceId state
            mbCurrentPatch = CState.currentPatch state
            pos = { left : coordX, top : coordY }
        whenJust2 mbCurrentPatch mbNodeR \curPatch nodeR -> do
            let moveNodeCommandOp = QOp.moveNode nodeR pos
            H.modify_
                $ CState.updateNodePosition (Patch.id curPatch) nodeR (Bounds.toNumberPosition pos)
                >>> CState.trackCommandOp moveNodeCommandOp
            sendNdfOpToWebSocket moveNodeCommandOp
    Ndf.Connect fromInstanceId outletId toInstanceId inletId -> pure unit
    Ndf.Disconnect fromInstanceId outletId toInstanceId inletId -> pure unit
    Ndf.Send instanceId inletId encodedVal -> pure unit
    Ndf.SendO instanceId outletId encodedVal -> pure unit
    Ndf.Order _ -> pure unit
    Ndf.Import _ -> pure unit
    Ndf.Comment _ -> pure unit
    Ndf.RemoveNode instanceId -> do
        -- FIXME: almost the same as: `handleAction ploc $ FromPatchArea Nothing PatchArea.RemoveNode`, we just need Id.NodeR for the existing node
        state <- H.get
        let
            mbNodeR = CState.findIdForNdfInstance instanceId state
            mbCurrentPatch = CState.currentPatch state
        whenJust2 mbCurrentPatch mbNodeR \curPatch nodeR -> do
            nextCurrentPatch <- H.lift $ Patch.disconnectAllFromTo nodeR curPatch
            let removeNodeCommandOp = QOp.removeNode nodeR
            H.modify_
                   $ CState.replacePatch (Patch.id curPatch) (nextCurrentPatch # Patch.removeNode nodeR)
                >>> CState.trackCommandOp removeNodeCommandOp
                >>> CState.forgetNdfInstance instanceId
            sendNdfOpToWebSocket removeNodeCommandOp
    Ndf.Documentation _ _ -> pure unit


panelSymbol
    :: forall loc tk ps fs sr cr m
     . MarkToolkit tk -- FIXME: get rid of typeclass constraints here, we don't need them to calculate symbol, and it's only for `Documentation` panel
    => HasChRepr tk cr
    => T.At At.Documentation cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => MonadEffect m
    => State loc tk ps fs sr cr m
    -> Panels.Which
    -> Char
panelSymbol state =
    case _ of
        Panels.Console       -> SidePanel.charOf SP.ConsoleLog.sidePanel state.log
        Panels.Commands      -> SidePanel.charOf SP.Commands.sidePanel state.history
        Panels.Tree          -> SidePanel.charOf SP.Tree.sidePanel state.network
        Panels.Documentation -> SidePanel.charOf SP.Documentation.sidePanel state
        Panels.WSStatus      -> SidePanel.charOf SP.WSStatus.sidePanel $ CState.loadWSState state
        Panels.HydraCode     -> SidePanel.charOf SP.HydraCode.sidePanel state.mbHydraProgram


panelSlot
    :: forall loc tk ps fs sr cr m
     . MarkToolkit tk
    => HasChRepr tk cr
    => T.At At.Documentation cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => MonadEffect m
    => SidePanel.RenderParams
    -> TargetLayer
    -> State loc tk ps fs sr cr m
    -> Panels.Which
    -> H.ComponentHTML (Action sr cr m) (Slots sr cr m) m
panelSlot params target state =
    case _ of
        Panels.Console       -> HH.slot_ _sidePanel (target /\ Panels.Console)       (SidePanel.panel target SP.ConsoleLog.panelId SP.ConsoleLog.sidePanel)       $ params /\ state.log
        Panels.Commands      -> HH.slot_ _sidePanel (target /\ Panels.Commands)      (SidePanel.panel target SP.Commands.panelId SP.Commands.sidePanel)           $ params /\ state.history
        Panels.Tree          -> HH.slot_ _sidePanel (target /\ Panels.Tree)          (SidePanel.panel target SP.Tree.panelId SP.Tree.sidePanel)                   $ params /\ state.network
        Panels.Documentation -> HH.slot_ _sidePanel (target /\ Panels.Documentation) (SidePanel.panel target SP.Documentation.panelId SP.Documentation.sidePanel) $ params /\ state
        Panels.WSStatus      -> HH.slot_ _sidePanel (target /\ Panels.WSStatus)      (SidePanel.panel target SP.WSStatus.panelId SP.WSStatus.sidePanel)           $ params /\ CState.loadWSState state
        Panels.HydraCode     -> HH.slot_ _sidePanel (target /\ Panels.HydraCode)     (SidePanel.panel target SP.HydraCode.panelId SP.HydraCode.sidePanel)         $ params /\ state.mbHydraProgram
