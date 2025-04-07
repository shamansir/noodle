module Web.Components.AppScreen where

import Prelude

import Type.Proxy (Proxy(..))

import Control.Monad.State (get, put, modify, modify_) as State
import Control.Monad.Extra (whenJust, whenJust2, whenJust_)

import Signal ((~>))
import Signal (runSignal) as Signal

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (toUnfoldable) as Map
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (unwrap) as NT
import Data.Text.Format (nil) as T

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Properties.Extra (Position(..), position) as HHP
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HS
import Halogen.Subscription as HSS

import Noodle.Wiring (class Wiring)
import Noodle.Id (PatchR, FamilyR, NodeR) as Id
import Noodle.Toolkit (Toolkit, class MarkToolkit)
import Noodle.Toolkit (families, class HoldsFamilies, class FromPatchState, spawnAnyRaw, loadFromPatch) as Toolkit
import Noodle.Network (toolkit, patches) as Network
import Noodle.Patch (make, id, name, findRawNode, registerRawNode, getState, allNodes, links, connectRaw, disconnectAllFromTo, removeNode) as Patch
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (run, _runOnInletUpdates, NodeChanges, id, setState, subscribeChanges) as RawNode
import Noodle.Repr.Tagged (class ValueTagged)
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.ChRepr (class WriteChannelRepr)
import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette
import Noodle.Ui.Tagging.At (ChannelLabel, StatusLine) as At
import Noodle.Ui.Tagging.At (class At) as T


import Web.Components.AppScreen.State (State)
import Web.Components.AppScreen.State
    ( init
    , spawnPatch, registerPatch, indexOfPatch, currentPatch, withCurrentPatch, replacePatch
    ) as CState
import Web.Components.PatchesBar as PatchesBar
import Web.Components.Library as Library
import Web.Components.PatchArea as PatchArea
import Web.Components.StatusBar as StatusBar
import Web.Class.WebRenderer (class WebLocator)
import Web.Layer (TargetLayer(..))


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
    | SelectPatch Id.PatchR
    | CreatePatch
    | SpawnNode Id.FamilyR
    | PassUpdate Id.PatchR Id.NodeR (RawNode.NodeChanges sr cr)
    | FromPatchesBar PatchesBar.Output
    | FromLibrary Library.Output
    | FromPatchArea PatchArea.Output


component
    :: forall query input output loc tk ps fs sr cr m
     . Wiring m
    => WebLocator loc
    => HasFallback cr
    => MarkToolkit tk
    => T.At At.ChannelLabel cr
    => T.At At.StatusLine cr
    => Toolkit.HoldsFamilies sr cr m fs
    => Toolkit.FromPatchState tk ps sr
    => ValueTagged cr
    => Proxy loc
    -> ps
    -> Toolkit tk fs sr cr m
    -> H.Component query input output m
component ploc pstate toolkit =
    H.mkComponent
        { initialState : initialState pstate toolkit
        , render : render ploc
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction pstate
            , initialize = Just Initialize
            }
        }


initialState :: forall input tk ps fs sr cr m. ps -> Toolkit tk fs sr cr m -> input -> State tk ps fs sr cr m
initialState pstate toolkit _ = CState.init pstate toolkit


render
    :: forall loc tk ps fs sr cr m
     . Wiring m
    => WebLocator loc
    => Toolkit.HoldsFamilies sr cr m fs
    => Toolkit.FromPatchState tk ps sr
    => MarkToolkit tk
    => HasFallback cr
    => ValueTagged cr
    => T.At At.StatusLine cr
    => T.At At.ChannelLabel cr
    => Proxy loc
    -> State tk ps fs sr cr m
    -> H.ComponentHTML (Action sr cr) (Slots sr cr m) m
render ploc state =
    HH.div
        [ HHP.position HHP.Abs { x : 0.0, y : 0.0 } ]
        [ HS.svg [ HSA.width width, HSA.height height ]
            [ HS.g
                []
                (
                    [ HS.rect
                        [ HSA.width width, HSA.height height
                        , HSA.fill $ Just $ P.hColorOf $ Palette.black
                        ]
                    , HS.g
                        [ HSA.transform [ HSA.Translate 0.0 0.0 ] ]
                        [ HH.slot _patchesBar unit PatchesBar.component patchesBarInput FromPatchesBar ]
                    , HS.g
                        [ HSA.transform [ HSA.Translate libraryX libraryY ] ]
                        [ HH.slot _library SVG (Library.component ptk SVG) libraryInput FromLibrary ]
                    , HS.g
                        [ HSA.transform [ HSA.Translate patchAreaX patchAreaY ] ]
                        [ HH.slot _patchArea unit (PatchArea.component ploc) patchAreaInput FromPatchArea ]
                    , HS.g
                        [ HSA.transform [ HSA.Translate 0.0 statusBarY ] ]
                        [ HH.slot_ _statusBar SVG (StatusBar.component SVG) statusBarInput ]
                    ]
                )
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
            width = 1000.0
            height = 1000.0
            (ptk :: _ tk) = Proxy
            curPatchNodes = CState.currentPatch state <#> Patch.allNodes # fromMaybe []
            curPatchLinks = CState.currentPatch state <#> Patch.links # fromMaybe []
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
                , state : state.initPatchesFrom
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
                } :: StatusBar.Input


handleAction
    :: forall output tk ps fs sr cr m
     . Wiring m
    => Toolkit.FromPatchState tk ps sr
    => HasFallback cr
    => ValueTagged cr
    => ps
    -> Action sr cr
    -> H.HalogenM (State tk ps fs sr cr m) (Action sr cr) (Slots sr cr m) output m Unit
handleAction pstate = case _ of
    Initialize -> do
        firstPatch <- H.lift $ Patch.make "Patch 1" pstate
        State.modify_ $ CState.registerPatch firstPatch
    CreatePatch -> do
        state <- State.get
        newPatch <- H.lift $ CState.spawnPatch state
        State.modify_ $ CState.registerPatch newPatch
        handleAction pstate $ FromPatchesBar $ PatchesBar.SelectPatch $ Patch.id newPatch
    SelectPatch patchR -> do
        state <- State.get
        H.modify_ _
            { currentPatch =
                CState.indexOfPatch patchR state
                    <#> (\pIndex -> { id : patchR, index : pIndex })
            }
    SpawnNode familyR -> do
        state <- State.get
        let toolkit = Network.toolkit state.network
        (mbRawNode :: Maybe (Raw.Node sr cr m)) <- H.lift $ Toolkit.spawnAnyRaw familyR toolkit
        whenJust mbRawNode \rawNode -> do
            let
                nodeR = RawNode.id rawNode
            H.lift $ RawNode._runOnInletUpdates rawNode

            mbCurrentPatch <- CState.currentPatch <$> State.get
            whenJust mbCurrentPatch \curPatch -> do
                (patchState :: ps) <- Patch.getState curPatch
                let (mbNodeState :: Maybe sr) = Toolkit.loadFromPatch (Proxy :: _ tk) familyR patchState
                whenJust mbNodeState
                    \nextState -> rawNode # RawNode.setState nextState

                _ <- H.subscribe =<< do
                    { emitter, listener } <- H.liftEffect HSS.create
                    H.liftEffect
                        $  Signal.runSignal
                        $  RawNode.subscribeChanges rawNode
                        ~> PassUpdate (Patch.id curPatch) nodeR
                        ~> HSS.notify listener
                    pure emitter

                H.modify_
                    $ CState.withCurrentPatch $ Patch.registerRawNode rawNode

                H.tell _patchArea unit $ PatchArea.ApplyNewNode rawNode

                H.lift $ RawNode.run rawNode
    PassUpdate patchR nodeR update ->
        State.get >>= CState.currentPatch >>> whenJust_ \curPatch ->
            when (Patch.id curPatch == patchR) $
                H.tell _patchArea unit $ PatchArea.ApplyUpdate nodeR update
    FromPatchesBar (PatchesBar.SelectPatch patchR) -> do
        handleAction pstate $ SelectPatch patchR
    FromPatchesBar PatchesBar.CreatePatch -> do
        handleAction pstate $ CreatePatch
    FromLibrary (Library.SelectFamily familyR) ->
        handleAction pstate $ SpawnNode familyR
    FromPatchArea (PatchArea.Connect (source /\ target)) -> do
        mbCurrentPatch <- CState.currentPatch <$> State.get
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
    FromPatchArea (PatchArea.UpdateStatusBar tag) ->
        H.modify_ _ { statusBarContent = Just tag }
    FromPatchArea PatchArea.ClearStatusBar ->
        H.modify_ _ { statusBarContent = Nothing }
    FromPatchArea (PatchArea.RemoveNode nodeR) -> do
        mbCurrentPatch <- CState.currentPatch <$> State.get
        whenJust mbCurrentPatch \curPatch -> do
            nextCurrentPatch <- H.lift $ Patch.disconnectAllFromTo nodeR curPatch
            H.modify_ $ CState.replacePatch (Patch.id curPatch) (nextCurrentPatch # Patch.removeNode nodeR)
