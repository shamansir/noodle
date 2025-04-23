module Web.Components.AppScreen where

import Prelude

import Debug as Debug

import Effect.Class (liftEffect)
import Effect.Console (log) as Console

import Type.Proxy (Proxy(..))

import Control.Monad.State (get, put, modify, modify_) as State
import Control.Monad.Extra (whenJust, whenJust2, whenJust_)

import Signal ((~>))
import Signal (runSignal) as Signal

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Map (empty, toUnfoldable) as Map
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (unwrap) as NT
import Data.Text.Format (nil) as T
import Data.Int (round, toNumber) as Int
import Data.String (toLower) as String
import Data.Traversable (traverse_)

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
import Noodle.Raw.Node (run, _runOnInletUpdates, NodeChanges, id, state, setState, subscribeChanges) as RawNode
import Noodle.Repr.Tagged (class ValueTagged)
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.ChRepr (class WriteChannelRepr)
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Fn.Signature (class PossiblyToSignature)
import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette
import Noodle.Ui.Tagging.At (ChannelLabel, StatusLine) as At
import Noodle.Ui.Tagging.At (class At) as T


import Web.Components.AppScreen.State (State)
import Web.Components.AppScreen.State
    ( init
    , spawnPatch, registerPatch, indexOfPatch, currentPatch, withCurrentPatch, replacePatch, currentPatchState'
    ) as CState
import Web.Components.PatchesBar as PatchesBar
import Web.Components.Library as Library
import Web.Components.PatchArea as PatchArea
import Web.Components.StatusBar as StatusBar
import Web.Class.WebRenderer (class WebLocator)
import Web.Layer (TargetLayer(..))

import HydraTk.Lang.Program (formProgram, printToJavaScript, class ToHydraCommand, collectHydraCommands) as Hydra -- FIXME
import HydraTk.Patch (resize) as Hydra -- FIXME


type Slots sr cr m =
    ( patchesBar :: forall q. H.Slot q PatchesBar.Output Unit
    , library :: forall q. H.Slot q Library.Output TargetLayer
    , patchArea :: H.Slot (PatchArea.Query sr cr m) PatchArea.Output Unit
    , statusBar :: H.Slot StatusBar.Query StatusBar.Output TargetLayer
    )


_library = Proxy :: _ "library"
_patchesBar = Proxy :: _ "patchesBar"
_patchArea = Proxy :: _ "patchArea"
_statusBar = Proxy :: _ "statusBar"


data Action sr cr
    = Initialize
    | GlobalKeyDown KeyboardEvent
    | GlobalKeyUp KeyboardEvent
    | SelectPatch Id.PatchR
    | CreatePatch
    | SpawnNode Id.FamilyR
    | PassUpdate Id.PatchR Id.NodeR (RawNode.NodeChanges sr cr)
    | FromPatchesBar PatchesBar.Output
    | FromLibrary Library.Output
    | FromPatchArea PatchArea.Output
    | FromStatusBar StatusBar.Output
    | HandleResize


component
    :: forall query input output loc tk ps fs sr cr m
     . Wiring m
    => WebLocator loc
    => HasFallback cr
    => MarkToolkit tk
    => T.At At.ChannelLabel cr
    => T.At At.StatusLine cr
    => Toolkit.HoldsFamilies sr cr m fs
    => Toolkit.InitPatchState tk ps m
    => Toolkit.FromToPatchState tk ps sr
    => HasChRepr tk cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => ValueTagged cr
    => Hydra.ToHydraCommand sr
    => Proxy loc
    -> Proxy ps
    -> Toolkit tk fs sr cr m
    -> H.Component query input output m
component ploc pps toolkit =
    H.mkComponent
        { initialState : initialState toolkit
        , render : render ploc pps
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
        }


initialState :: forall input tk ps fs sr cr m. Toolkit tk fs sr cr m -> input -> State tk ps fs sr cr m
initialState toolkit _ = CState.init toolkit


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
    => ValueTagged cr
    => HasChRepr tk cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => T.At At.StatusLine cr
    => T.At At.ChannelLabel cr
    => Proxy loc
    -> Proxy ps
    -> State tk ps fs sr cr m
    -> H.ComponentHTML (Action sr cr) (Slots sr cr m) m
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
                    (
                        [ HS.rect
                            [ HSA.width width, HSA.height height
                            , HSA.fill $ Just backgroundWithAlpha -- FIXME: `bgOpacity` for PatchArea & AppScreen multiples
                            ]
                        , HS.g
                            [ HSA.transform [ HSA.Translate 0.0 0.0 ] ]
                            [ HH.slot _patchesBar unit PatchesBar.component patchesBarInput FromPatchesBar ]
                        , HS.g
                            [ HSA.transform [ HSA.Translate libraryX libraryY ] ]
                            [ HH.slot _library SVG (Library.component ptk SVG) libraryInput FromLibrary ]
                        , HS.g
                            [ HSA.transform [ HSA.Translate patchAreaX patchAreaY ] ]
                            [ HH.slot _patchArea unit (PatchArea.component ptk ploc) patchAreaInput FromPatchArea ]
                        , HS.g
                            [ HSA.transform [ HSA.Translate 0.0 statusBarY ] ]
                            [ HH.slot _statusBar SVG (StatusBar.component SVG) statusBarInput FromStatusBar ]
                        ]
                    )
                ]
            ]
        , HH.div_
            [ HH.div
                [ HHP.position HHP.Abs { x : statusBarX, y : statusBarY } ]
                [ HH.slot_ _statusBar HTML (StatusBar.component HTML) statusBarInput ]
            , HH.div
                [ HHP.position HHP.Abs { x : libraryX, y : libraryY } ]
                [ HH.slot _library HTML (Library.component ptk HTML) libraryInput FromLibrary ]
            ]
        ]
        where
            solidBackground = P.hColorOf Palette.black
            backgroundWithAlpha = fromMaybe solidBackground $ HCColorX.setAlpha state.bgOpacity solidBackground
            width  = fromMaybe 1000.0 $ _.width  <$> state.size
            height = fromMaybe 1000.0 $ _.height <$> state.size
            (ptk :: _ tk) = Proxy
            curPatchNodes = CState.currentPatch state <#> Patch.allNodes # fromMaybe []
            curPatchLinks = CState.currentPatch state <#> Patch.links # fromMaybe []
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

            libraryInput = { families : Toolkit.families $ Network.toolkit state.network } :: Library.Input
            patchAreaInput =
                { offset : { left : patchAreaX, top : patchAreaY }
                , size : { width : patchAreaWidth, height : patchAreaHeight }
                , zoom : state.zoom
                , bgOpacity : state.bgOpacity
                , mbState : curPatchState
                , nodes : curPatchNodes
                , links : curPatchLinks
                } :: PatchArea.Input ps sr cr m
            patchesBarInput =
                { patches : map Patch.name <$> (Map.toUnfoldable $ Network.patches state.network)
                , selected : _.id <$> state.currentPatch
                } :: PatchesBar.Input
            statusBarInput =
                { content : fromMaybe T.nil state.statusBarContent
                , width : statusBarWidth
                , currentZoom : state.zoom
                } :: StatusBar.Input


handleAction
    :: forall output tk ps fs sr cr m
     . Wiring m
    => Toolkit.InitPatchState tk ps m
    => Toolkit.FromToPatchState tk ps sr
    => Hydra.ToHydraCommand sr
    => HasFallback cr
    => ValueTagged cr
    => Action sr cr
    -> H.HalogenM (State tk ps fs sr cr m) (Action sr cr) (Slots sr cr m) output m Unit
handleAction = case _ of
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

        handleAction HandleResize
    HandleResize -> do
        window <- H.liftEffect $ Web.window
        newWidth <- H.liftEffect $ Window.innerWidth window
        newHeight <- H.liftEffect $ Window.innerHeight window
        H.modify_ $ _ { size = Just { width : Int.toNumber newWidth, height : Int.toNumber newHeight } }
        H.liftEffect $ Hydra.resize newWidth newHeight
        pure unit
    CreatePatch -> do
        state <- H.get
        newPatch <- H.lift $ CState.spawnPatch state
        H.modify_ $ CState.registerPatch newPatch.state newPatch.patch
        handleAction $ FromPatchesBar $ PatchesBar.SelectPatch $ Patch.id newPatch.patch
    SelectPatch patchR -> do
        state <- H.get
        H.modify_ _
            { currentPatch =
                CState.indexOfPatch patchR state
                    <#> (\pIndex -> { id : patchR, index : pIndex })
            }
    SpawnNode familyR -> do
        state <- H.get
        let toolkit = Network.toolkit state.network
        (mbRawNode :: Maybe (Raw.Node sr cr m)) <- H.lift $ Toolkit.spawnAnyRaw familyR toolkit
        whenJust mbRawNode \rawNode -> do
            let
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
                    $ CState.withCurrentPatch $ Patch.registerRawNode rawNode

                Patch.trackStateChangesFromRaw (Proxy :: _ tk) rawNode curPatch

                H.tell _patchArea unit $ PatchArea.ApplyNewNode rawNode

                H.lift $ RawNode.run rawNode
    PassUpdate patchR nodeR update ->
        H.get >>= CState.currentPatch >>> whenJust_ \curPatch -> do
            when (Patch.id curPatch == patchR) $
                H.tell _patchArea unit $ PatchArea.ApplyUpdate nodeR update
            collectedCommands <- H.lift $ Hydra.collectHydraCommands curPatch
            H.liftEffect $ Console.log $ Hydra.printToJavaScript $ Hydra.formProgram collectedCommands
    FromPatchesBar (PatchesBar.SelectPatch patchR) -> do
        handleAction $ SelectPatch patchR
    FromPatchesBar PatchesBar.CreatePatch -> do
        handleAction $ CreatePatch
    FromLibrary (Library.SelectFamily familyR) ->
        handleAction $ SpawnNode familyR
    FromPatchArea (PatchArea.TryZoom dy) -> do
        state <- H.get
        when state.shiftPressed $
            H.put $ state { zoom = min 3.0 $ max 0.3 $ state.zoom + (dy * 0.1) }
    FromPatchArea (PatchArea.Connect (source /\ target)) -> do
        mbCurrentPatch <- CState.currentPatch <$> H.get
        whenJust mbCurrentPatch \curPatch -> do
            whenJust2 (Patch.findRawNode source.fromNode curPatch) (Patch.findRawNode target.toNode curPatch)
                \srcNode dstNode -> do
                    nextPatch /\ _ <-
                        H.lift $ Patch.connectRaw
                            source.fromOutlet
                            target.toInlet
                            srcNode
                            dstNode
                            curPatch
                    H.modify_ $ CState.replacePatch (Patch.id curPatch) nextPatch
    FromPatchArea (PatchArea.Disconnect linkR) -> do
        mbCurrentPatch <- CState.currentPatch <$> H.get
        whenJust mbCurrentPatch \curPatch -> do
            whenJust (Patch.findRawLink linkR curPatch) \rawLink -> do
                nextPatch /\ _ <- H.lift $ Patch.disconnectRaw rawLink curPatch
                H.modify_ $ CState.replacePatch (Patch.id curPatch) nextPatch
    FromPatchArea (PatchArea.UpdateStatusBar tag) ->
        H.modify_ _ { statusBarContent = Just tag }
    FromPatchArea PatchArea.ClearStatusBar ->
        H.modify_ _ { statusBarContent = Nothing }
    FromPatchArea (PatchArea.RemoveNode nodeR) -> do
        mbCurrentPatch <- CState.currentPatch <$> H.get
        whenJust mbCurrentPatch \curPatch -> do
            nextCurrentPatch <- H.lift $ Patch.disconnectAllFromTo nodeR curPatch
            H.modify_ $ CState.replacePatch (Patch.id curPatch) (nextCurrentPatch # Patch.removeNode nodeR)
    FromStatusBar StatusBar.ResetZoom ->
        H.modify_ $ _ { zoom = 1.0 }
    GlobalKeyDown kevt -> do
        H.modify_ $ _ { shiftPressed = KE.shiftKey kevt }
        when (String.toLower (KE.key kevt) == "escape") $ H.tell _patchArea unit PatchArea.CancelConnecting
    GlobalKeyUp kevt ->
        H.modify_ $ _ { shiftPressed = KE.shiftKey kevt }
