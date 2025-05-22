module Web.Components.AppScreen.State where

import Prelude

import Effect.Class (class MonadEffect)

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (empty, insert, lookup) as Map
import Data.Traversable (traverse)
import Data.Text.Format (Tag) as T
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (length, snoc) as Array

import Noodle.Id (PatchR, NodeR) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (class InitPatchState, initPatch) as Toolkit
import Noodle.Toolkit.Families (Families)
import Noodle.Patch (Patch)
import Noodle.Patch (id, make, getState) as Patch
import Noodle.Network (Network)
import Noodle.Network (init, patchesCount, patch, addPatch, withPatch) as Network

import HydraTk.Lang.Program (Program) as Hydra

import Web.Components.ValueEditor (Def) as ValueEditor
import Web.Components.HelpText (Context(..)) as HelpText
import Web.Components.PatchArea (LockingTask(..), NodesBounds) as PatchArea
import Web.Components.SidePanel.Console (LogLine(..)) as Console


data UiMode
    = CanvasFullyVisible
    | TransparentOverlay Number -- semi-transparent overlay over canvas
    | SolidOverlay UiMode -- solid color over the canvas (canvas not visible), keeps previous mode to get back to it
    | OnlyCanvas UiMode -- UI is hidden, keeps previous mode to get back to it


type State (tk :: ToolkitKey) ps (fs :: Families) sr cr m =
    { size :: Maybe { width :: Number, height :: Number }
    , zoom :: Number
    , uiMode :: UiMode
    , helpText :: Boolean
    , shiftPressed :: Boolean
    , network :: Network tk ps fs sr cr m
    , patchIdToIndex :: Map Id.PatchR PatchIndex
    , mbCurrentPatch :: Maybe { index :: PatchIndex, id :: Id.PatchR }
    , mbCurrentPatchState :: Maybe ps -- FIXME: it's data duplication, but we store it here, because getting it from Network is effectful and we need it on `render` for the user's Patch component
    , mbStatusBarContent :: Maybe T.Tag
    , mbHydraProgram :: Maybe Hydra.Program -- FIXME : should be created by Hydra toolkit itself
    , mbCurrentEditor :: Maybe (Id.NodeR /\ ValueEditor.Def cr)
    , commandInputActive :: Boolean
    , nodesBounds :: Map Id.PatchR PatchArea.NodesBounds -- FIXME: data duplication as well, choose where we want to store them for sure, maybe since we pass them through HTML/SVG anyway, store everything in `AppScreen` is better
    , lastCurPatchLock :: PatchArea.LockingTask -- FIXME: also data duplication for `LockingTask`, but we need to know what `PatchArea` performs now to show the corresponding help
    , log :: Array Console.LogLine
    }


newtype PatchIndex = PatchIndex Int


init :: forall tk ps fs sr cr m. Toolkit tk fs sr cr m -> State tk ps fs sr cr m
init toolkit =
    { size : Nothing
    , zoom : 1.0
    , uiMode : TransparentOverlay 0.1
    , shiftPressed : false
    , helpText : true
    , network : Network.init toolkit
    , patchIdToIndex : Map.empty
    , mbCurrentPatch : Nothing
    , mbCurrentPatchState : Nothing
    , mbStatusBarContent : Nothing
    , mbHydraProgram : Nothing
    , mbCurrentEditor : Nothing
    , commandInputActive : false
    , lastCurPatchLock : PatchArea.NoLock
    , nodesBounds : Map.empty
    , log : []
    }


spawnPatch :: forall tk ps fs sr cr mp m. MonadEffect m => Toolkit.InitPatchState tk ps m => State tk ps fs sr cr mp -> m { state :: ps, patch :: Patch ps fs sr cr mp }
spawnPatch s = do
    let
        patchesCount = s.network # Network.patchesCount
        nextPatchIndex = patchesCount + 1
    patchState <- Toolkit.initPatch (Proxy :: _ tk)
    Patch.make ("Patch " <> show nextPatchIndex) patchState
        <#> \newPatch -> { patch : newPatch, state : patchState }


registerPatch :: forall tk ps fs sr cr m. ps -> Patch ps fs sr cr m -> State tk ps fs sr cr m -> State tk ps fs sr cr m
registerPatch patchState newPatch s =
    let
        patchesCount = s.network # Network.patchesCount
        nextPatchIndex = PatchIndex $ patchesCount + 1
        nextNW = s.network # Network.addPatch newPatch
        patchR = Patch.id newPatch
    in
        s
            { mbCurrentPatch = Just { index : nextPatchIndex, id : patchR } -- FIXME: make patch current in a separate function
            , mbCurrentPatchState = Just patchState
            , patchIdToIndex = s.patchIdToIndex # Map.insert patchR nextPatchIndex
            , network = nextNW
            , nodesBounds = s.nodesBounds # Map.insert patchR Map.empty
            }


lastPatchIndex :: forall tk ps fs sr cr m. State tk ps fs sr cr m -> PatchIndex
lastPatchIndex s = PatchIndex $ Network.patchesCount s.network


indexOfPatch :: forall tk ps fs sr cr m. Id.PatchR -> State tk ps fs sr cr m -> Maybe PatchIndex
indexOfPatch patchR = _.patchIdToIndex >>> Map.lookup patchR


patch :: forall tk ps fs sr cr m. Id.PatchR -> State tk ps fs sr cr m -> Maybe (Patch ps fs sr cr m)
patch patchR = _.network >>> Network.patch patchR


currentPatch :: forall tk ps fs sr cr m. State tk ps fs sr cr m -> Maybe (Patch ps fs sr cr m)
currentPatch s = s.mbCurrentPatch <#> _.id >>= flip patch s


currentPatchId :: forall tk ps fs sr cr m. State tk ps fs sr cr m -> Maybe Id.PatchR
currentPatchId s = s.mbCurrentPatch <#> _.id


currentPatchState :: forall tk ps fs sr cr mp m. MonadEffect m => State tk ps fs sr cr mp -> m (Maybe ps)
currentPatchState = traverse Patch.getState <<< currentPatch


currentPatchState' :: forall tk ps fs sr cr m. State tk ps fs sr cr m -> Maybe ps
currentPatchState' = _.mbCurrentPatchState


withPatch :: forall tk ps fs sr cr m. Id.PatchR -> (Patch ps fs sr cr m -> Patch ps fs sr cr m) -> State tk ps fs sr cr m -> State tk ps fs sr cr m
withPatch patchR f s = s { network = Network.withPatch patchR f s.network }


replacePatch :: forall tk ps fs sr cr m. Id.PatchR -> Patch ps fs sr cr m -> State tk ps fs sr cr m -> State tk ps fs sr cr m
replacePatch patchR = withPatch patchR <<< const


withCurrentPatch :: forall tk ps fs sr cr m. (Patch ps fs sr cr m -> Patch ps fs sr cr m) -> State tk ps fs sr cr m -> State tk ps fs sr cr m
withCurrentPatch f s = case s.mbCurrentPatch <#> _.id of
    Just curPatchR -> withPatch curPatchR f s
    Nothing -> s


type PatchStats =
    { lockOn :: PatchArea.LockingTask
    , nodesCount :: Int
    , linksCount :: Int
    }


extractHelpContext :: forall tk ps fs sr cr m. State tk ps fs sr cr m -> PatchStats -> HelpText.Context
extractHelpContext state pStats =
    if state.commandInputActive then HelpText.CommandInputOpen
    else case state.mbCurrentEditor of
        Just _ ->
            HelpText.EnteringValue
        Nothing ->
            case state.uiMode of
                OnlyCanvas _ ->
                    HelpText.InterfaceHidden
                _ ->
                    case pStats.lockOn of
                        PatchArea.DraggingNode _ ->
                            HelpText.DraggingNode
                        PatchArea.Connecting _ _ ->
                            HelpText.CreatingLink
                        PatchArea.NoLock ->
                            HelpText.Start
                                { hasLinks : pStats.linksCount > 0
                                , hasNodes : pStats.nodesCount > 0
                                , zoomChanged : state.zoom /= 1.0
                                }


log :: forall tk ps fs sr cr m. String -> State tk ps fs sr cr m -> State tk ps fs sr cr m
log logLine s = s { log = Array.snoc s.log $ Console.LogLine logLine }


logSome :: forall tk ps fs sr cr m. Array String -> State tk ps fs sr cr m -> State tk ps fs sr cr m
logSome logLines s = s { log = s.log <> (Console.LogLine <$> logLines) }